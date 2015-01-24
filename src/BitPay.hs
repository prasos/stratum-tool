{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module BitPay (bitpay, simpleRate) where

import Control.Applicative
import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever,mzero)
import Data.Aeson
import Data.Aeson.Types
import Data.Map as M (Map, fromList, lookup, keys)
import Data.Text as T (Text, toUpper, toLower, intercalate, unpack)
import Network.Curl.Aeson

import Retry

-- To maintain compatibility with aeson < 0.7, we use Value instead of
-- Scientific.
type RateMap = Map Text Value

ratesToMap :: Value -> Parser RateMap
ratesToMap v = do
  pairs <- parseJSON v >>= mapM rateToPair
  return $ fromList pairs

rateToPair :: Value -> Parser (Text, Value)
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

-- |Look for exchange rate for the given currency.
simpleRate :: TVar RateMap -> Text -> IO (Text, Value)
simpleRate var code = do
  m <- readTVarIO var
  case M.lookup code m of
    Nothing -> fail $ "Unknown currency code. Supported codes: " ++
               (T.unpack $ T.intercalate ", " $ map T.toUpper $ M.keys m)
    Just  x -> return (code,x)
