module Trees where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = case f x of
    Just (x1, x2, x3) -> Node (unfold f x1) x2 (unfold f x3)
    Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
    where f :: Integer -> Maybe (Integer, Integer, Integer)
          f x = case x >= n of
                    True -> Nothing
                    False -> Just (x + 1, x, x + 1)

-- The below essentially worked, but built the tree in
-- the wrong order -- descending from the node instead of
-- ascending as the book desired
--          f n = case n <= 0 of
--                    True -> Nothing
--                    False -> Just (n - 1, n - 1, n - 1)
