module Main where

import Criterion.Main
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as S

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

-- Note: this (and dumpEnqToDeq) might be better if I limited
-- the number of elements they would reverse and place into
-- the dequeue to some fixed number, like 100. From my
-- benchmarks, it seems as if doing one large `reverse` call
-- on a list of, say, 10,000 elements takes more than 10x as
-- long as 10 `reverse` calls on lists of 1,000 elements. That
-- is odd, though -- would it imply that `reverse` has 
-- worse-than-linear complexity? I would have thought that it
-- had precisely linear complexity.
fromList :: [a] -> Queue a
fromList xs = Queue [] (reverse xs)

empty :: Queue a
empty = fromList []

dumpEnqToDeq :: Queue a -> Queue a
dumpEnqToDeq (Queue enq deq) = Queue [] (deq ++ reverse enq)

push :: a -> Queue a -> Queue a
push x (Queue enq deq) = Queue (x:enq) deq

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue enq (y:ys)) = Just (y, Queue enq ys)
pop q@(Queue (x:xs) []) = pop (dumpEnqToDeq q)
pop (Queue [] []) = Nothing

pushL :: a -> [a] -> [a]
pushL = (:)

popL :: [a] -> Maybe (a, [a])
popL [] = Nothing
popL l = Just (last l, init l)

pushS :: a -> S.Seq a -> S.Seq a
pushS x s = S.singleton x S.>< s

popS :: S.Seq a -> Maybe (a, S.Seq a)
popS s 
  | null s = Nothing
  | otherwise = Just (lastEl, rest)
    where lastIndex = S.length s - 1
          lastEl = S.index s lastIndex
          rest = S.deleteAt lastIndex s

altPushPopQueue :: Int -> Queue Int
altPushPopQueue n = go n queue
  where queue = fromList [500..999]
        go x q
          | x <= 0 = q
          | otherwise = go (x-1) pushedPopped
          where pushed = push x q
                pushedPopped = fromMaybe empty $ snd <$>
                                 pop pushed

altPushPopList :: Int -> [Int]
altPushPopList n = go n list
  where list = [500..999]
        go x l
          | x <= 0 = l
          | otherwise = go (x-1) pushedPopped
          where pushed = pushL x l
                pushedPopped = fromMaybe [] $ snd <$>
                                 popL pushed

altPushPopSeq :: Int -> S.Seq Int
altPushPopSeq n = go n seq
  where seq = S.fromList [500..999]
        go x s
          | x <= 0 = s
          | otherwise = go (x-1) pushedPopped
          where pushed = pushS x s
                pushedPopped = fromMaybe S.empty $ snd <$>
                                 popS pushed

-- The good news is that, based on my benchmarks, it's clear
-- that the queue is roughly 4x faster than the list; the
-- downside is that my pushS and popS methods must not be
-- very good, because they perform markedly worse than a list
-- (either that or, while sequences are good at appending
-- to the end, they're actually pretty bad at popping from
-- the end)
main :: IO ()
main = defaultMain
  [ bench "push to, pop from list" $
    whnf altPushPopList 10000
  , bench "push to, pop from queue" $
    whnf altPushPopQueue 10000
  , bench "push to, pop from seq" $
    whnf altPushPopSeq 10000
  ]
