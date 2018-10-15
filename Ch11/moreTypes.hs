module MoreTypes where

--data Person = MkPerson String Int deriving (Eq, Show)

data Person = Person { name :: String
                     , age :: Int }
                     deriving (Eq, Show)

jm = Person "Julie" 108
ca = Person "Chris" 16

namae :: Person -> String
namae (Person s _) = s


--data Fiction = Fiction deriving Show
--data Nonfiction = Nonfiction deriving Show

--data BookType = FictionBook Fiction
--              | NonfictionBook Nonfiction
--              deriving Show

type AuthorName = String

--data Author = Author (AuthorName, BookType)

data Author = Fiction AuthorName
            | Nonfiction AuthorName
            deriving (Eq, Show)

--data FlowerType = Gardenia
--                | Daisy
--                | Rose
--                | Lilac
--                deriving Show
--
--data Garden =
--    Garden Gardener FlowerType
--    deriving Show

type Gardener = String

data Garden = Gardenia Gardener
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener
            deriving Show
