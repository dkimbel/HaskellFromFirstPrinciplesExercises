module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

multRecur :: (Eq a, Num a) => a -> a -> a
multRecur 0 _ = 0
multRecur _ 0 = 0
multRecur x y = x + multRecur x (y - 1)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) =>
               Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [a, b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 :: Integer) + (1 :: Integer) > (1 :: Integer) `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      (2 :: Integer) + (2 :: Integer) `shouldBe` (4 :: Integer)
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + (1 :: Integer) > (x :: Integer)
  describe "Division" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy (15 :: Integer) (3 :: Integer) `shouldBe` 
        (5 :: Integer, 0 :: Integer)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy (22 :: Integer) (5 :: Integer) `shouldBe` 
        (4 :: Integer, 2 :: Integer)
  describe "Multiplication" $ do
    it "3 times 5 is 15" $ do
      multRecur (3 :: Integer) (5 :: Integer) `shouldBe`
        (15 :: Integer)
    it "10 times 0 is 0" $ do
      multRecur (10 :: Integer) (0 :: Integer) `shouldBe`
        (0 :: Integer)
