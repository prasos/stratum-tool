{-# LANGUAGE OverloadedStrings #-}
-- |Pretty-print JSON as human-readable breadcrumbs.
module PrettyJson where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Builder
import qualified Data.HashMap.Strict as H
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Vector as V

breadcrumbs :: Value -> Builder
breadcrumbs = breadcrumbs' True mempty mempty

breadcrumbs' :: Bool -> Builder -> Builder -> Value -> Builder
breadcrumbs' start path b v = case v of
  Object o -> H.foldlWithKey' (objBuilder start path) b o
  Array a -> V.ifoldl (arrayBuilder path) b a
  _ -> b <>
       path <>
       (if start then mempty else byteString " = ") <>
       lazyByteString (encode v) <>
       byteString "\n"

objBuilder :: Bool -> Builder -> Builder -> Text -> Value -> Builder
objBuilder start path b k v = breadcrumbs' False newPath b v
  where newPath = path <>
                  (if start then mempty else byteString ".") <>
                  buildText k

arrayBuilder :: Builder -> Builder -> Int -> Value -> Builder
arrayBuilder path b i v = breadcrumbs' False newPath b v
  where newPath = path <>
                  byteString "[" <>
                  string7 (show i) <>
                  byteString "]"

buildText :: Text -> Builder
buildText = byteString . encodeUtf8
