{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson
import Data.ByteString.Lazy.Builder -- Remove Lazy when using newer bytestring lib
import Data.Monoid
import Data.String (fromString)
import System.Console.CmdArgs.Implicit
import System.IO (stdout)

import StratumClient
import PrettyJson

data Args = Args { server   :: String
                 , port     :: Int
                 , command  :: String
                 , params   :: [String]
                 , multi    :: Bool
                 , raw      :: Bool
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
       , raw = def &=
               help "Output as raw JSON instead of JSON breadcrumbs format"
       }
  &= program "stratum-tool"
  &= summary "StratumTool v0.0.1"
  &= help "Connect to Electrum server via Stratum protocol and \
          \allows querying wallet balances etc."

main = do
  Args{..} <- cmdArgs synopsis
  stratumConn <- connectStratum server $ fromIntegral port
  ans <- if multi
         then objectZip params <$>
              mapConcurrently (queryStratumValue stratumConn command . pure) params
         else queryStratumValue stratumConn command params
  hPutBuilder stdout $ if raw
                       then lazyByteString (encode ans) <> byteString "\n"
                       else breadcrumbs ans

-- |Pairs a given list of strings corresponding values to generate
-- JSON object with string as a key.
objectZip :: [String] -> [Value] -> Value
objectZip ss vs = object $ zipWith toPair ss vs
  where toPair s v = (fromString s, v)
