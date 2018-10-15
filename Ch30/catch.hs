module Catch where

import Control.Exception

canICatch :: Exception e
          => e
          -> IO (Either ArithException ())
canICatch e =
  try $ throwIO e

catchAll :: Exception e
         => e
         -> IO (Either SomeException ())
catchAll e =
  try $ throwIO e
