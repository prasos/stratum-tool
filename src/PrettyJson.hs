-- |Pretty-print JSON as human-readable breadcrumbs.
module PrettyJson where

import Data.Aeson
import Data.Aeson.Types
import Data.Text.Encoding
import Data.ByteString.Lazy.Builder -- Remove Lazy when using newer bytestring lib
import Data.Monoid
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H

breadcrumbs :: Value -> Builder
breadcrumbs = breadcrumbs' True mempty mempty

breadcrumbs' :: Bool -> Builder -> Builder -> Value -> Builder
breadcrumbs' start path b v = case v of
  Object o -> H.foldlWithKey' (objBuilder start path) b o
  Array a -> V.ifoldl (arrayBuilder path) b a
  _ -> b <>
       path <>
       string7 " = " <>
       lazyByteString (encode v) <>
       char7 '\n'

objBuilder :: Bool -> Builder -> Builder -> Text -> Value -> Builder
objBuilder start path b k v = breadcrumbs' False newPath b v
  where newPath = path <>
                  (if start then mempty else char7 '.') <>
                  buildText k

arrayBuilder :: Builder -> Builder -> Int -> Value -> Builder
arrayBuilder path b i v = breadcrumbs' False newPath b v
  where newPath = path <>
                  char7 '[' <>
                  string7 (show i) <>
                  char7 ']'

buildText :: Text -> Builder
buildText = byteString . encodeUtf8
