{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module BitPay (bitpay, simpleRate, Scientific) where

import Control.Applicative
import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever,mzero)
import Data.Aeson
import Data.Aeson.Types
import Data.Map as M (Map,fromList,lookup)
import Data.Text (Text, toLower)
import Network.Curl.Aeson
import Data.Scientific (Scientific)

import Retry

type RateMap = Map Text Scientific

ratesToMap :: Value -> Parser RateMap
ratesToMap v = do
  pairs <- parseJSON v >>= mapM rateToPair
  return $ fromList pairs

rateToPair :: Value -> Parser (Text,Scientific)
rateToPair = withObject "Not an object" $ \v -> do
  k <- v .: "code"
  v <- v .: "rate"
  return (toLower k,v)

getRates :: IO RateMap
getRates = curlAesonGetWith ratesToMap "https://bitpay.com/api/rates"

bitpayLoop :: TVar RateMap -> IO ()
bitpayLoop var = forever $ do
  threadDelay 60000000
  xs <- foreverRetryPrintEx getRates
  atomically $ writeTVar var xs

bitpay :: IO (TVar RateMap)
bitpay = do
  initial <- getRates
  var <- newTVarIO initial
  forkIO $ bitpayLoop var
  return var

simpleRate :: TVar RateMap -> Text -> IO (Text, Scientific)
simpleRate var code = do
  m <- readTVarIO var
  case M.lookup code m of
    Nothing -> fail "Unknown currency code"
    Just x  -> return (code,x)
