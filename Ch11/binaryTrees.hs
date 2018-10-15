module BinaryTrees where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' x Leaf = Node Leaf x Leaf
insert' x (Node left y right)
    -- I originally treated didn't no-op on the x == y case,
    -- and rather treated it the same as the < case; otherwise
    -- I was able to create this successfully by myself, though
    | x == y = Node left y right
    | x < y  = Node (insert' x left) y right
    | x > y  = Node left y (insert' x right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left x right) =
    Node (mapTree f left) (f x) (mapTree f right)

-- I came up with the map tree correctly by myself, without
-- using any of the book's hints. The test code below was
-- provided by the book.
testTree' :: BinaryTree Integer
testTree' = 
    Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
    Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = 
    if mapTree (+1) testTree' == mapExpected
    then putStrLn "MapTree fine!"
    else putStrLn "Bad news bears."

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = 
    [x] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) =
    inorder left ++ [x] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) =
    postorder left ++ postorder right ++ [x]

-- I would have liked to come up with a version that
-- didn't depend on converting to a list first, but
-- I can't find a way to and I'm not sure whether it's
-- actually possible
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f z tree = foldr f z (inorder tree)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad news bears."

testFoldTree :: IO ()
testFoldTree =
    if foldTree (+) 0 testTree == 6
    then putStrLn "FoldTree fine!"
    else putStrLn "Bad news bears."

main :: IO ()
main = do 
    mapOkay
    testPreorder
    testInorder
    testPostorder
    testFoldTree
