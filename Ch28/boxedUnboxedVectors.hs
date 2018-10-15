module Main where

import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UBV

mapperBoxed :: Int -> V.Vector Int
mapperBoxed n = V.map (+1) ns
  where ns = V.fromList [1..n]

mapperUnboxed :: Int -> UBV.Vector Int
mapperUnboxed n = UBV.map (+1) ns
  where ns = UBV.fromList [1..n]

main :: IO ()
main = defaultMain
  [ bench "map over large vector" $
    whnf mapperBoxed 987654
  , bench "map over large unboxed vector" $
    whnf mapperUnboxed 987654
  ]
