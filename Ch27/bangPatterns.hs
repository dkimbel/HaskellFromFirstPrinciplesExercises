{-# LANGUAGE BangPatterns #-}

module ManualBang where

doesntEval :: Bool -> Int
doesntEval b = 1

manualSeq :: Bool -> Int
manualSeq b = b `seq` 1

banging :: Bool -> Int
banging !b = 1

data Foo = Foo Int !Int
  deriving Show

first :: Foo -> Int
first (Foo x _) = x

second :: Foo -> Int
second (Foo _ y) = y

data DoesntForce =
  TisLazy Int String
  deriving Show

gibString :: DoesntForce -> String
gibString (TisLazy _ s) = s

data BangBang =
  SheShotMeDown !Int !String
  deriving Show

gimmeString :: BangBang -> String
gimmeString (SheShotMeDown _ s) = s
