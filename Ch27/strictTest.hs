{-# LANGUAGE Strict #-}

module StrictTest where

blah :: Int -> Int
blah x = 1

main :: IO ()
main = print (blah undefined)
