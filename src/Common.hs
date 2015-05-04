{-# LANGUAGE DeriveDataTypeable #-}
module Common where

import System.Console.CmdArgs.Implicit (Data, Typeable)

data Security = Tcp
              | SafeSsl
              | Ssl
              deriving (Show, Eq, Data, Typeable)

data Follow = OneShot
            | ShootAndFollow
            | FollowOnly
            deriving (Show, Eq, Data, Typeable)
