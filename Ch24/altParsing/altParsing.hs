{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString =
  Either Integer String

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

a :: String
a = "blah"

b :: String
b = "123"

c :: String
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = 
 skipMany (oneOf "\n")
 >>
     (Left <$> integer)
 <|> (Right <$> some letter)
 <* 
 skipMany (oneOf "\n")

class Applicative f => Alternative' f where
  empty' :: f a
  (<||>) :: f a -> f a -> f a

  some' :: f a -> f [a]
  some' v = some_v
    where 
      -- Can this definition really work? it's straight
      -- from the book, but it feels to me like it would
      -- recurse infinitely since some_v and many_v are
      -- referring to each other. Maybe this is allowed
      -- in some special way by the compiler since we're
      -- inside of a typeclass definition?
      many_v = some_v <||> pure []
      some_v = (fmap (:) v) <*> many_v

  many' :: f a -> f [a]
  many' v = many_v
    where 
      many_v = some_v <||> pure []
      some_v = (fmap (:) v) <*> many_v

main = do
  let p f i =
        parseString f mempty i
--  print $ p (some letter) a
--  print $ p integer b
--  print $ p parseNos a
--  print $ p parseNos b
--  print $ p (many parseNos) c
--  print $ p (some parseNos) c
  print $ p (many parseNos) eitherOr
