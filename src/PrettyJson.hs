{-# LANGUAGE OverloadedStrings #-}
-- |Pretty-print JSON as human-readable breadcrumbs.
module PrettyJson where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Builder
import qualified Data.Map as M
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Vector as V

breadcrumbs :: String -> Value -> Builder
breadcrumbs = breadcrumbs' True mempty mempty

breadcrumbs' :: Bool -> Builder -> Builder -> String -> Value -> Builder
breadcrumbs' start path b delimiter v = case v of
  Object o -> let m = takeJSON (Object o)
                  objPath = path <> if start then mempty else char7 '.'
              in if M.null m
                 then item $ byteString "{}"
                 else M.foldlWithKey (objBuilder delimiter objPath) b m
  Array a -> if V.null a
             then item $ byteString "[]"
             else V.ifoldl (arrayBuilder delimiter path) b a
  _ -> item $ lazyByteString $ encode v
  where item x = b <>
                 path <>
                 (if start then mempty else byteString " = ") <>
                 lazyByteString (encode v) <>
                 stringUtf8 delimiter

objBuilder :: String -> Builder -> Builder -> Text -> Value -> Builder
objBuilder delimiter path b k v = breadcrumbs' False newPath b delimiter v
  where newPath = path <>
                  buildText k

arrayBuilder :: String -> Builder -> Builder -> Int -> Value -> Builder
arrayBuilder delimiter path b i v = breadcrumbs' False newPath b delimiter v
  where newPath = path <>
                  byteString "[" <>
                  string7 (show i) <>
                  byteString "]"

buildText :: Text -> Builder
buildText = byteString . encodeUtf8

-- |Simple wrapper for fromJSON in case you know what should be inside
-- the JSON. Error is raised if types are not matching.
takeJSON :: FromJSON a => Value -> a
takeJSON x = case fromJSON x of
  Error e   -> error e
  Success a -> a
