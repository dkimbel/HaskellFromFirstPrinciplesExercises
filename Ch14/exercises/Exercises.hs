module Exercises where

import Data.Char (toUpper)
import Data.List (sort)
import Test.Hspec
import Test.QuickCheck
import WordNumber (digitToWord, digits, wordNumber)

data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

genFool :: Gen Fool
genFool = oneof [return Fulse, return Frue]

genFoolWeighted :: Gen Fool
genFoolWeighted =
  frequency [ (2, return Fulse)
            , (1, return Frue)]

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

square :: Num a => a -> a
square x = x * x

squareAndRoot :: Floating a => a -> a
squareAndRoot = square . sqrt

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (c:cs) = toUpper c : cs

listOrdered :: Ord a => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y =
  x + y == y + x

multAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
multAssociative x y z =
  x * (y * z) == (x * y) * z

multCommutative :: (Num a, Eq a) => a -> a -> Bool
multCommutative x y =
  x * y == y * x

expAssociative :: (Num a, Integral a, Eq a) => 
                  a -> a -> a -> Bool
expAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z

expCommutative :: (Num a, Integral a, Eq a) => 
                  a -> a -> Bool
expCommutative x y =
  x ^ y == y ^ x

quotRemLossless :: (Integral a, Eq a) => a -> a -> Bool
quotRemLossless x y =
  (quot x y) * y + (rem x y) == x

divModLossless :: (Integral a, Eq a) => a -> a -> Bool
divModLossless x y =
  (div x y) * y + (mod x y) == x

prop_quotRemLossless :: Property
prop_quotRemLossless =
  forAll (limitedIntGen)
    (\x y -> quotRemLossless (y :: Int) x == True)

limitedIntGen :: Gen Int
limitedIntGen = elements [1..1000]

doubleReverse :: [a] -> [a]
doubleReverse = reverse . reverse

dollarSign :: Eq b => (a -> b) -> a -> Bool
dollarSign f x = (f $ x) == f x

funcComp :: Eq c => (b -> c) -> (a -> b) -> a -> Bool
funcComp f g x = (f . g $ x) == f (g x)

-- Note: I tried to mock out the function instead of hardcoding succ,
-- but was not successful; the type signature of coarbitrary seemed
-- not to be helpful, because I didn't want a Gen as a return value
prop_dollarSign :: Property
prop_dollarSign =
  forAll (arbitrary :: Gen Int)
    (\x -> dollarSign succ x == True)

-- Likewise, I would have preferred to use some kind of Gen solution
-- for the functions instead of hardcoding them
prop_funcComp :: Property
prop_funcComp =
  forAll (arbitrary :: Gen Int)
    (\x -> funcComp succ pred x == True)

twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

runQc :: IO ()
runQc = do
  quickCheck prop_quotRemLossless
  quickCheck prop_dollarSign
  quickCheck prop_funcComp

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"
    it "returns INVALID for 10" $ do
      digitToWord 10 `shouldBe` "INVALID"

  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]

  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one given 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"

  describe "Arithmetic" $ do
    it "number equals itself if halved and doubled" $ do
      property $ \x -> halfIdentity x == (x :: Double)
    it "addition is associative" $ do
      property $ \x y z -> plusAssociative (x :: Int) 
                                           (y :: Int) 
                                           (z :: Int) == True
    it "addition is commutative" $ do
      property $ \x y -> plusCommutative (x :: Int) 
                                         (y :: Int) == True
    it "multiplication is associative" $ do
      property $ \x y z -> multAssociative (x :: Int) 
                                           (y :: Int) 
                                           (z :: Int) == True
    it "multiplication is commutative" $ do
      property $ \x y -> multCommutative (x :: Int) 
                                         (y :: Int) == True
--    it "square doesn't square" $ do
--      property $ \x -> square (x :: Int) == x
--    it "square undoes sqrt" $ do
--      property $ \x -> squareAndRoot (x :: Float) == x
--    it "exponentiation is associative" $ do
--      property $ \x y z -> expAssociative (x :: Int) 
--                                          (y :: Int)
--                                          (z :: Int) == True
--    it "exponentiation is commutative" $ do
--      property $ \x y -> expCommutative (x :: Int) 
--                                        (y :: Int) == True

  describe "Lists" $ do
    it "a sorted list should be sorted, ascending" $ do
      property $ \xs -> listOrdered (sort xs :: [Int]) == True
    it "a list, twice reversed, is unchanged" $ do
      property $ \xs -> doubleReverse (xs :: [Int]) == xs
--    it "take should retrieve length's worth of elements" $ do
--      property $ \n xs -> length (take (n :: Int) (xs :: [Int])) ==
--                          (n :: Int)

  describe "Folds" $ do
    it "foldr (:) should be equivalent to (++)" $ do
      property $ \xs ys -> foldr (:) (xs :: [Int]) (ys :: [Int]) == 
                           (++) (ys :: [Int]) (xs :: [Int])
    it "foldr (++) should be equivalent to concat" $ do
      property $ \xs -> foldr (++) [] (xs :: [[Int]]) == 
                        concat (xs :: [[Int]])

  describe "Read/Show" $ do
    it "read and show should complete round trip for Ints" $ do
      property $ \x -> read (show (x :: Int)) == x
    it "read and show should complete round trip for Bools" $ do
      property $ \x -> read (show (x :: Bool)) == x

  describe "Idempotence" $ do
    it "sort is idempotent" $ do
      property $ \x -> ((sort (x :: [Int]) == (twice sort) x)) 
                       && (sort x == (fourTimes sort) x)
    it "capitalizeWord is idempotent" $ do
      property $ \x -> ((capitalizeWord (x :: String) == 
                        (twice capitalizeWord) x)) 
                       && (capitalizeWord x == 
                       (fourTimes capitalizeWord) x)
