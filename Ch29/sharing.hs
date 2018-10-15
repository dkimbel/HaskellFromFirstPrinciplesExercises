module Sharing where

import Debug.Trace

mySharedVal :: String
mySharedVal = trace "evaluated mySharedVal" "hiya"

func1 :: Int -> [String]
func1 n = take n $ repeat mySharedVal

otherVal :: String
otherVal = reverse mySharedVal
