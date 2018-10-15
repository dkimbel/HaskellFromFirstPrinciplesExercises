module Main where

import Criterion.Main

myList :: [Int]
myList = [1..9999]
--myList = undefined : [2..9999]
--myList = undefined : undefined
--myList = undefined

-- In my actual benchmarking run, "badMap" was barely worse
-- than map; it was a difference of about 1%.
badMap :: (a -> b) -> [a] -> [b]
badMap _ [] = []
badMap f (x:xs) = [f x] ++ (map f xs)

main :: IO ()
main = defaultMain
  [ bench "map list 9999" $
--    whnf (map (+1)) myList
    nf (map (+1)) myList
  , bench "badMap list 9999" $
    nf (badMap (+1)) myList
  ]
