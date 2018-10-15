module Main where

import Criterion.Main
import qualified Data.Vector as V

testV' :: Int -> V.Vector Int
testV' n =
  V.map (+n) $ V.map (+n) $
    V.map (+n) $ V.map (+n)
    (V.fromList [1..10000])

testV :: Int -> V.Vector Int
testV n =
  V.map ( (+n) . (+n)
        . (+n) . (+n) )
        (V.fromList [1..10000])

testLFourPass :: Int -> [Int]
testLFourPass n =
  map (+n) $ map (+n) $
    map (+n) $ map (+n) [1..10000]

testLOnePass :: Int -> [Int]
testLOnePass n =
  map ( (+n) . (+n)
      . (+n) . (+n) )
      [1..10000]
    

main :: IO ()
main = defaultMain
  -- Note: the idea here is that testV is already written
  -- to only do one pass, because there's only a single
  -- call to V.map. However, testV' is written to call
  -- V.map four times, thus doing four passes and building
  -- four new vectors. HOWEVER, because Data.Vector has
  -- loop fusion built in, the would-be four-pass version
  -- only does one pass, and testV' has benchmarks that
  -- are virutally identical to testV.
  -- I get the impression that linked lists also have
  -- stream fusion built in, because I'm also getting
  -- identical performance for my own testLFourPass
  -- and testLOnePass methods.
  [ bench "vector map prefused" $
      whnf testV 9998
  , bench "vector map will be fused" $
      whnf testV' 9998
  , bench "list map four passes" $
      nf testLFourPass 9998
  , bench "list map one pass" $
      nf testLOnePass 9998
  ]
