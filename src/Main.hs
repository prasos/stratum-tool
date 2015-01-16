{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import Data.Aeson
import System.Console.CmdArgs.Implicit
import qualified Data.ByteString.Lazy.Char8 as B
import StratumClient

data Args = Args { server   :: String
                 , port     :: Int
                 , command  :: String
                 , params   :: [String]
                 } deriving (Show, Data, Typeable)

synopsis = Args { server = "electrum.bittiraha.fi" &=
                           help "Electrum server address (default: electrum.bittiraha.fi)"
                , port = 50001 &= help "Electrum port (default: 50001)"
                , command = def &= argPos 0 &= typ "COMMAND"
                , params = def &= args &= typ "PARAMS"
                }
           &= program "stratumtool"
           &= summary "StratumTool v0.0.1"
           &= help ("Connect to Electrum server via Stratum protocol and " ++
                    "allows querying wallet balances etc.")

main = do
  Args{..} <- cmdArgs synopsis
  stratumConn <- connectStratum server $ fromIntegral port
  ans <- queryStratum stratumConn command params
  B.putStrLn $ encode (ans::Value)
