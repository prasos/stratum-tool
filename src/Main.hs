{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM (atomically, readTChan)
import Data.Aeson
import Data.ByteString.Builder
import qualified Data.Map as M
import Data.Monoid
import Data.String (fromString)
import System.Console.CmdArgs.Implicit
import System.IO

import StratumClient
import PrettyJson

data Args = Args { server   :: String
                 , port     :: Int
                 , command  :: String
                 , params   :: [String]
                 , multi    :: Bool
                 , json     :: Bool
                 , follow   :: Bool
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
       }
  &= program "stratum-tool"
  &= summary "StratumTool v0.0.2"
  &= help "Connect to Electrum server via Stratum protocol and \
          \allows querying wallet balances etc."

main = do
  args@Args{..} <- cmdArgs synopsis
  stratumConn <- connectStratum server $ fromIntegral port
  hSetBuffering stdout LineBuffering
  (if follow then trackAddresses else oneTime) stratumConn args

-- |Track changes in given addresses and run the command when changes
-- occur.
trackAddresses :: StratumConn -> Args -> IO ()
trackAddresses stratumConn Args{..} = do
  chan <- stratumChan stratumConn "blockchain.address.subscribe"
  -- Subscribe, but throw away results (only some hashes there)
  hashes <- mapConcurrently (qv "blockchain.address.subscribe" . pure) params
  values <- mapConcurrently (qv command . pure) params
  let m = M.fromList $ zipWith mapify params hashes
  -- Print current state at first
  oneTime stratumConn Args{multi=True,..}
  -- Listen for changes
  let loop m = do
        [addr,newHash] <- takeJSON <$> atomically (readTChan chan)
        if m M.! addr /= newHash
          then do newValue <- qv command [addr]
                  printValue json $ object [fromString addr .= newValue]
                  loop $ M.insert addr newHash m
          else loop m
    in loop m
  where qv = queryStratumValue stratumConn
        mapify a h = (a, takeJSON h)

-- |Process single request. 
oneTime :: StratumConn -> Args -> IO ()
oneTime stratumConn Args{..} = do
  ans <- if multi
         then objectZip params <$>
              mapConcurrently (queryStratumValue stratumConn command . pure) params
         else queryStratumValue stratumConn command params
  printValue json ans

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
