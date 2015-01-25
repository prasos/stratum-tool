{-# LANGUAGE DeriveDataTypeable #-}
module Common where

import System.Console.CmdArgs.Implicit (Data, Typeable)

data Security = Tcp
              | UnsafeSsl
              | Ssl
              deriving (Show, Data, Typeable)
