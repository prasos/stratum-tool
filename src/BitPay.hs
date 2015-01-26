{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module BitPay (initBitpay, simpleRate) where

import Control.Applicative
import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever,mzero)
import Data.Aeson
import Data.Aeson.Types
import Data.Map as M hiding (map)
import Data.Text as T (Text, toUpper, toLower, intercalate, unpack)
import Data.Time.Clock.POSIX
import Network.Curl.Aeson
import Network.Curl.Opts

-- To maintain compatibility with aeson < 0.7, we use Value instead of
-- Scientific.
type RateMap = Map Text Value

ratesToMap :: Value -> Parser RateMap
ratesToMap v = do
  pairs <- parseJSON v >>= mapM rateToPair
  return $ fromList pairs

rateToPair :: Value -> Parser (Text, Value)
rateToPair (Object v) = do
  k <- v .: "code"
  v <- v .: "rate"
  return (toLower k,v)
rateToPair _ = fail "Not an object"

getRates :: IO RateMap
getRates = curlAeson ratesToMap "GET" "https://bitpay.com/api/rates"
           [CurlTimeout 30] noData

-- |Return an action which returns the rates when the action is
-- run. This doesn't perform HTTP query so it's OK to run this even if
-- no currency conversion is needed. The returned action caches rates
-- for 60 seconds and fails if BitPay has been unreachable for 10
-- minutes.
initBitpay :: IO (IO RateMap)
initBitpay = do
  lastUpdateVar <- newTVarIO 0
  ratesVar <- newTVarIO M.empty
  return $ do
    lastUpdate <- readTVarIO lastUpdateVar
    now <- getPOSIXTime
    -- Use cache if rates are less than 1 minute old
    if now < lastUpdate + 60
      then readTVarIO ratesVar
      else do rateTry <- try getRates
              case rateTry of
                Right newRates -> atomically $ do
                  writeTVar lastUpdateVar now
                  writeTVar ratesVar newRates
                  return newRates
                Left e -> do
                  putStrLn $ "BitPay unreachable: " ++ show (e :: CurlAesonException)
                  -- If cache is older than 10 minutes,
                  -- fail. Otherwise returning old data.
                  if now < lastUpdate + 600
                    then readTVarIO ratesVar
                    else fail "No fresh cached data available, quitting"

-- |Look for exchange rate for the given currency.
simpleRate :: RateMap -> Text -> (Text, Value)
simpleRate m code =
  case M.lookup code m of
    Nothing -> error $ "Unknown currency code. Supported codes: " ++
               (T.unpack $ T.intercalate ", " $ map T.toUpper $ M.keys m)
    Just x -> (code, x)
