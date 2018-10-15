module WhatHappensEvil where

import Control.Concurrent
import System.IO.Unsafe

myData :: MVar Int
myData = unsafePerformIO newEmptyMVar

main :: IO ()
main = do
  putMVar myData 0
  zero <- takeMVar myData
  print zero
