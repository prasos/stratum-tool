{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module StratumClient where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Aeson
import Network
import System.IO
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as I
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

type StratumQuery = Value -> IO ()

data StratumConn = StratumConn { h         :: Handle
                               , listeners :: TVar (IntMap StratumQuery)
                               , channels  :: TVar (Map String (TChan Value))
                               , nextSeq   :: TVar Int
                               }

data Response = Reply Int Value | Push String Value deriving (Show)

instance FromJSON Response where
  parseJSON (Object o) = (Reply <$> o .: "id" <*> o .: "result") <|>
                         (Push <$> o .: "method" <*> o .: "params")

connectStratum :: HostName -> PortNumber -> IO StratumConn
connectStratum host port = do
  h <- connectTo host $ PortNumber port
  hSetBuffering h LineBuffering
  listeners <- newTVarIO $ I.empty
  channels <- newTVarIO $ M.empty
  forkIO $ forever $ do
    json <- B.hGetLine h
    case eitherDecodeStrict json of
      Left e -> hPutStr stderr $ "JSON parsing error: " ++ e
      -- Send reply to the request sender
      Right (Reply i v) -> do
        mbF <- atomically $ listenerMapTake listeners i
        case mbF of
          Just f -> f v
          Nothing -> hPutStr stderr "Unknown ID in server message"
      -- Push the message to a subscription channel
      Right (Push k v) -> atomically $ channelMapPut channels k v
  nextSeq <- newTVarIO 0
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
queryStratum :: FromJSON a => StratumConn -> String -> [String] -> IO a
queryStratum StratumConn{..} method params = do
  out <- newEmptyTMVarIO
  i <- atomically $ do
    i <- takeId nextSeq
    modifyTVar listeners $ I.insert i $ atomically . putTMVar out
    return i
  BL.hPut h $ encode $ object [ "id" .= i
                              , "method" .= method
                              , "params" .= params
                              ]
  B.hPut h "\n"
  value <- atomically $ takeTMVar out
  case fromJSON value of
    Error s -> fail s
    Success a -> return a

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

-- |Helper for ignoring JSON output (asks for a Value so it will
-- success and throw it away if the returned JSON is valid)
ignoreJSON :: Value -> IO ()
ignoreJSON _ = return ()
