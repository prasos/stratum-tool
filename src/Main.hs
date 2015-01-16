{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import Control.Applicative
import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson
import System.Console.CmdArgs.Implicit
import qualified Data.ByteString.Lazy.Char8 as B
import StratumClient

data Args = Args { server   :: String
                 , port     :: Int
                 , command  :: String
                 , params   :: [String]
                 , multi    :: Bool
                 } deriving (Show, Data, Typeable)

synopsis = Args { server = "electrum.bittiraha.fi" &=
                           help "Electrum server address (default: electrum.bittiraha.fi)"
                , port = 50001 &= help "Electrum port (default: 50001)"
                , command = def &= argPos 0 &= typ "COMMAND"
                , params = def &= args &= typ "PARAMS"
                , multi = def &=
                          help "Instead of passing multiple parameters for \
                               \a single command, repeat command for each \
                               \argument (default: false)"
                }
           &= program "stratumtool"
           &= summary "StratumTool v0.0.1"
           &= help ("Connect to Electrum server via Stratum protocol and " ++
                    "allows querying wallet balances etc.")

main = do
  Args{..} <- cmdArgs synopsis
  stratumConn <- connectStratum server $ fromIntegral port
  ans <- if multi
         then toJSON <$> mapConcurrently (queryStratumValue stratumConn command . pure) params
         else queryStratumValue stratumConn command params
  B.putStrLn $ encode ans
