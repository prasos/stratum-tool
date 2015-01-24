{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module BitPay where

import Control.Applicative
import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever,mzero)
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map,fromList)
import Network.Curl.Aeson

import Retry

ratesToMap :: Value -> Parser (Map String Double)
ratesToMap v = do
  pairs <- parseJSON v >>= mapM rateToPair
  return $ fromList pairs

rateToPair :: Value -> Parser (String,Double)
rateToPair = withObject "Not an object" $ \v -> do
  k <- v .: "code"
  v <- v .: "rate"
  return (k,v)

getRates :: IO (Map String Double)
getRates = curlAesonGetWith ratesToMap "https://bitpay.com/api/rates"

bitpayLoop :: TVar (Map String Double) -> IO ()
bitpayLoop var = forever $ do
  threadDelay 60000000
  xs <- foreverRetryPrintEx getRates
  atomically $ writeTVar var $ xs

bitpay :: IO (TVar (Map String Double))
bitpay = do
  initial <- getRates
  var <- newTVarIO initial
  forkIO $ bitpayLoop var
  return var
