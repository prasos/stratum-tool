{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module StratumClient where

-- NB! Data.IntMap.Lazy and Data.Map.Lazy are imported without Lazy
-- because they default to Lazy and the naming has changed in recent
-- versions of containers. By doing this we maintain
-- backwards-compatibility

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Network (HostName, PortNumber)
import Network.Connection
import System.IO
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Data.Map (Map)
import qualified Data.Map as M
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Common

type StratumQuery = Value -> IO ()

data StratumConn = StratumConn { sender    :: TChan ByteString
                               , listeners :: TVar (IntMap StratumQuery)
                               , channels  :: TVar (Map String (TChan Value))
                               , nextSeq   :: TVar Int
                               }

data Response = Reply Int (Either String Value) | Push String Value deriving (Show)

instance FromJSON Response where
  parseJSON (Object o) = (Reply <$> o .: "id" <*> (Right <$> o .: "result")) <|>
                         (Reply <$> o .: "id" <*> (Left <$> o .: "error")) <|>
                         (Push <$> o .: "method" <*> o .: "params")

jsonMax = 10^8 -- 100 megabytes of JSON is too much for us

connectStratum :: HostName -> PortNumber -> Security -> IO StratumConn
connectStratum host port security = do
  ctx <- initConnectionContext
  conn <- connectTo ctx $ ConnectionParams host port
          (case security of
              Tcp     -> Nothing
              Ssl     -> Just $ TLSSettingsSimple True False False
              SafeSsl -> Just $ TLSSettingsSimple False False False
          )
          Nothing
  sender <- newTChanIO
  listeners <- newTVarIO $ I.empty
  channels <- newTVarIO $ M.empty
  nextSeq <- newTVarIO 0
  -- Thread for parsing output
  forkIO $ forever $ do
    json <- connectionGetLine jsonMax conn
    case decode (BL.fromChunks [json]) of
      Nothing -> hPutStr stderr $ "JSON parsing error"
      -- Send reply to the request sender
      Just (Reply i payload) -> do
        mbF <- atomically $ listenerMapTake listeners i
        case mbF of
          Just f -> case payload of
            Right v -> f v
            Left e  -> f $ error e
          Nothing -> hPutStr stderr "Unknown ID in server message"
      -- Push the message to a subscription channel
      Just (Push k v) -> atomically $ channelMapPut channels k v
  -- Thread for sending data
  forkIO $ forever $ do
    bs <- atomically $ readTChan sender
    mapM_ (connectionPut conn) $ BL.toChunks $ bs `BL.snoc` '\n'
  -- Thread for pinging
  forkIO $ forever $ do
    threadDelay 300000000
    queryStratumValue StratumConn{..} "server.version" ["1.9.8", "0.9"]
  return StratumConn{..}

-- |Take element from a transactional map
listenerMapTake :: TVar (IntMap a) -> I.Key -> STM (Maybe a)
listenerMapTake mapVar k = do
  m <- readTVar mapVar
  writeTVar mapVar $ I.delete k m
  return $ I.lookup k m

-- |Put element into channel of given key. If there is no such
-- channel, the value is thrown away.
channelMapPut :: Ord k => TVar (Map k (TChan a)) -> k -> a -> STM ()
channelMapPut mapVar k v = do
  m <- readTVar mapVar
  case M.lookup k m of
    Just chan -> writeTChan chan v
    Nothing -> return ()

-- |Take value from transactional variable and increment it afterwards.
takeId :: TVar Int -> STM Int
takeId var = do
  i <- readTVar var
  modifyTVar' var succ
  return i

-- |Send request to Stratum server and wait for response
queryStratumParse :: (Value -> Parser a) -> StratumConn -> String -> [String] -> IO a
queryStratumParse p StratumConn{..} method params = do
  out <- newEmptyTMVarIO
  atomically $ do
    i <- takeId nextSeq
    modifyTVar listeners $ I.insert i $ atomically . putTMVar out
    writeTChan sender $ encode $ object [ "id" .= i
                                        , "method" .= method
                                        , "params" .= params
                                        ]
  value <- atomically $ takeTMVar out
  case parse p value of
    Error s -> fail s
    Success a -> return a

-- |Like queryStratumParse but use FromJSON to decide parser
queryStratum :: FromJSON a => StratumConn -> String -> [String] -> IO a
queryStratum = queryStratumParse parseJSON

-- |Like queryStratumParse but doesn't even parse anything
queryStratumValue :: StratumConn -> String -> [String] -> IO Value
queryStratumValue = queryStratumParse return

-- |Registers a listener for given channel name
stratumChan :: StratumConn -> String -> IO (TChan Value)
stratumChan StratumConn{..} name = atomically $ do
  m <- readTVar channels
  case M.lookup name m of
    Just bc -> dupTChan bc
    Nothing -> do
      bc <- newBroadcastTChan
      modifyTVar channels $ M.insert name bc
      dupTChan bc

-- |Helper for printing channel output
foreverDump :: Show a => TChan a -> IO b
foreverDump chan = forever $ do
  a <- atomically $ readTChan chan
  print a
