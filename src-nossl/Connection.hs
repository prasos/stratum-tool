-- |Connection module for SSL disabled builds which don't have
-- connection library.
module Connection where

import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Network
import System.IO
import Common

type Conn = Handle

sslEnabled :: Bool
sslEnabled = False

connOpen :: HostName -> PortNumber -> Security -> IO Conn
connOpen host port security = do
  when (security /= Tcp) $ fail "Builds for Debian Wheezy and Ubuntu Precise lack SSL support. Use `-S tcp` option to turn off encryption."
  h <- connectTo host $ PortNumber port
  hSetBuffering h LineBuffering
  return h

connGetLine :: Conn -> IO BS.ByteString
connGetLine = BS.hGetLine

connPutLazy :: Conn -> BL.ByteString -> IO ()
connPutLazy h bs = do
  BL.hPut h bs
  hFlush h
