-- |Connection module for SSL enabled builds
module Connection where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Network hiding (connectTo)
import Network.Connection
import Common

type Conn = Connection

sslEnabled :: Bool
sslEnabled = True

connOpen :: HostName -> PortNumber -> Security -> IO Conn
connOpen host port security = do
  ctx <- initConnectionContext
  connectTo ctx $ ConnectionParams host port
    (case security of
        Tcp     -> Nothing
        Ssl     -> Just $ TLSSettingsSimple True False False
        SafeSsl -> Just $ TLSSettingsSimple False False False
    )
    Nothing

connGetLine :: Conn -> IO BS.ByteString
connGetLine = connectionGetLine lineMax
  where lineMax = 10^8 -- 100 megabytes of JSON is too much for us

connPutLazy :: Conn -> BL.ByteString -> IO ()
connPutLazy c bs = mapM_ (connectionPut c) $ BL.toChunks bs
