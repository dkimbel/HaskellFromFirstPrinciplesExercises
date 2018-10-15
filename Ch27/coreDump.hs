module CoreDump where

discriminatory :: Bool -> Int
discriminatory b =
  let x = undefined
  in case x `seq` b of
    False -> 0
    True  -> 1

-- To see Core output in GHCi, use these two flags:
-- :set -ddump-simpl
-- :set -dsuppress-all
-- The latter simplifies the output.

-- The two functions below are NOT the same in core
doubA :: Int -> Int
doubA i = i * 2

doubB :: Int -> Int
doubB i = i + i
