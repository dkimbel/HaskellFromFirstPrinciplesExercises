{-# LANGUAGE RecordWildCards #-}

module RWCDemo where

data Blah =
  Blah { myThing :: Int }
  deriving Show

wew :: Blah -> IO ()
wew Blah{..} = print myThing

verbose :: Blah -> IO ()
verbose blah = print $ myThing blah

pattern :: Blah -> IO ()
pattern (Blah thing) = print thing
