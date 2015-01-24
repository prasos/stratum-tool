{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM (atomically, readTChan)
import Data.Aeson
import Data.ByteString.Builder
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Monoid
import Data.String (fromString)
import Data.Text as T (Text, pack, toLower)
import System.Console.CmdArgs.Implicit
import System.IO

import BitPay
import StratumClient
import PrettyJson

type InjectorSource = IO (Value -> Value)

data Args = Args { server   :: String
                 , port     :: Int
                 , command  :: String
                 , params   :: [String]
                 , multi    :: Bool
                 , json     :: Bool
                 , follow   :: Bool
                 , currency :: String
                 } deriving (Show, Data, Typeable)

synopsis =
  Args { server = "electrum.bittiraha.fi" &=
                  help "Electrum server address (electrum.bittiraha.fi)" &=
                  typ "HOST"
       , port = 50001 &= help "Electrum port (50001)"
       , command = def &= argPos 0 &= typ "COMMAND"
       , params = def &= args &= typ "PARAMS"
       , multi = def &=
                 help "Instead of passing multiple parameters for a single \
                      \command, repeat command for each argument"
       , json = def &=
                help "Output as raw JSON instead of JSON breadcrumbs format"
       , follow = def &=
                  help "Subscribe to given addresses and run given command \
                       \when something happens. Implies --multi."
       , currency = def &= typ "CODE" &=
                    help "Convert bitcoins to given currency using BitPay. \
                         \All currency codes supported by BitPay are available."
       }
  &= program "stratum-tool"
  &= summary "StratumTool v0.0.3"
  &= help "Connect to Electrum server via Stratum protocol and \
          \allows querying wallet balances etc."

main = do
  args@Args{..} <- cmdArgs synopsis
  stratumConn <- connectStratum server $ fromIntegral port
  hSetBuffering stdout LineBuffering
  rateVar <- bitpay
  let getInjector = if null currency
                    then return id
                    else currencyInjector <$>
                         (simpleRate rateVar $ T.toLower $ T.pack currency)
  (if follow then trackAddresses else oneTime) getInjector stratumConn args

-- |Track changes in given addresses and run the command when changes
-- occur.
trackAddresses :: InjectorSource -> StratumConn -> Args -> IO ()
trackAddresses getInjector stratumConn Args{..} = do
  chan <- stratumChan stratumConn "blockchain.address.subscribe"
  -- Subscribe and collect the hashes for future comparison
  hashes <- mapConcurrently (qv "blockchain.address.subscribe" . pure) params
  -- Print current state at first
  oneTime getInjector stratumConn Args{multi=True,..}
  -- Listen for changes
  let loop m = do
        [addr,newHash] <- takeJSON <$> atomically (readTChan chan)
        if m M.! addr /= newHash
          then do newValue <- qv command [addr]
                  injector <- getInjector
                  printValue json $ injector $
                    object [fromString addr .= newValue]
                  loop $ M.insert addr newHash m
          else loop m
    in loop $ M.fromList $ zipWith mapify params hashes
  where qv = queryStratumValue stratumConn
        mapify a h = (a, takeJSON h)

-- |Process single request. 
oneTime :: InjectorSource -> StratumConn -> Args -> IO ()
oneTime getInjector stratumConn Args{..} = do
  ans <- if multi
         then objectZip params <$>
              mapConcurrently (queryStratumValue stratumConn command . pure) params
         else queryStratumValue stratumConn command params
  injector <- getInjector
  printValue json $ injector ans

-- |Prints given JSON value to stdout. When `json` is True, then just
-- print as encoded to JSON, otherwise breadcrumbs format is used.
printValue :: Bool -> Value -> IO ()
printValue json ans =
  hPutBuilder stdout $ if json
                       then lazyByteString (encode ans) <> byteString "\n"
                       else breadcrumbs ans

-- |Pairs a given list of strings corresponding values to generate
-- JSON object with string as a key.
objectZip :: [String] -> [Value] -> Value
objectZip ss vs = object $ zipWith toPair ss vs
  where toPair s v = (fromString s, v)

-- |Inject currency data recursively to given Value.
currencyInjector :: (Text, Value) -> Value -> Value
currencyInjector rate v = case v of
  Object o -> Object $ H.fromList $ map conv $ H.toList o
  Array a -> Array $ V.map (currencyInjector rate) a
  _ -> v
  where
    conv (k, (Number n)) | k `elem` currencyFields = (k, inject rate (Number n))
    conv (k, v) = (k, (currencyInjector rate) v)

-- |List of Stratum object key names which contain bitcoin amounts.
currencyFields :: [Text]
currencyFields = ["confirmed"
                 ,"unconfirmed"
                 ,"value"
                 ]

-- |Converts given numeric value to Object containing amount in
-- satoshis and given currency.
inject :: (Text, Value) -> Value -> Value
inject (code, Number rate) (Number n) =
  object [("satoshi", Number n)
         ,(code, Number (n*rate*1e-8))
         ]
