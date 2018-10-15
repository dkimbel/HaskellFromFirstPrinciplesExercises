module HigherKindedTypes where

data Silly a b c d = MkSilly a b c d deriving Show

-- example of making your own infix data constructor
data Product a b = a :&: b deriving (Eq, Show)

-- a list type without using infix syntax
data List a = Nil | Cons a (List a) deriving Show
