module CheckerMonads where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  quickBatch (monad [(1, 2, 3) :: (Int, Int, Int)])
