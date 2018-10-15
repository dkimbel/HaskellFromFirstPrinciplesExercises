module Main where

import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S

bumpIt :: (Int, Int) -> (Int, Int)
bumpIt (x, y) = (x + 1, y + 1)

mapA :: M.Map Int Int
mapA = M.fromList $ take 10000 $ iterate bumpIt (0, 0)

mapB :: M.Map Int Int
mapB = M.fromList $ take 10000 $ iterate bumpIt (5000, 5000)

setA :: S.Set Int
setA = S.fromList $ take 10000 $ iterate (+1) 0

setB :: S.Set Int
setB = S.fromList $ take 10000 $ iterate (+1) 5000

mapUnionA :: M.Map Int Int -> M.Map Int Int
mapUnionA m = M.union mapA m

setUnionA :: S.Set Int -> S.Set Int
setUnionA s = S.union setA s

main :: IO ()
main = defaultMain
  [ bench "map union" $
    whnf mapUnionA mapB
  , bench "set union" $
    whnf setUnionA setA
  ]
