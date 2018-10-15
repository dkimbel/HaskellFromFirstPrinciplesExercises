import Data.List (elemIndex)

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y' :: Maybe Integer
y' = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z' :: Maybe Integer
z' = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y' <*> z'

x'' :: Maybe Int
x'' = elemIndex 3 [1,2,3,4,5]

y'' :: Maybe Int
y'' = elemIndex 4 [1,2,3,4,5]

-- the book included this, but I've confirmed that it's
-- completely unnecessary
max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x'' <*> y''

xs = [1, 2, 3]
ys = [4, 5, 6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys

y :: Maybe Integer
y = lookup 2 $ zip xs ys

-- note: sticking to the book's requirement of using
-- that tuple data constructor, I don't see how it's 
-- possible to actually find the sum of both x and y's
-- integers without doing more than simply adding 
-- uses of `pure`, `<$>`, and `<*>`. It would work
-- just fine if we were dealing with a list and not
-- a tuple.
summed :: Maybe Integer
--summed = (+) <$> x <*> y
summed = fmap sum $ ((,) <$> x <*> y)

