module RandomExample where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie' rollDie' rollDie'

-- as described by the book, this function doesn't do what we
-- want; it repeats a single die value, not the action that
-- produces a die value
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie'

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie'

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen

-- exercise
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= n = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 [] g
  where go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
        go sum count log gen
          | sum >= n = (count, log)
          | otherwise =
            let (dieVal, nextGen) = randomR (1, 6) gen
            in go (sum + dieVal) 
                  (count + 1) 
                  (log ++ [intToDie dieVal])
                  nextGen

-- code from book's earlier "pre-State-monad" example below;
-- book's code using the State monad is above

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- As the book notes, it's only appropriate to use
    -- error-throwing code like this in experiments or when
    -- a code branch is absolutely impossible to call.
    -- Instead, in real code, use Maybe or Either.
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  -- Note: I've confirmed that the `do` syntax the book is
  -- using here is completely unnecessary. I think it'd be
  -- better to delete `do` and instead put `in` right before
  -- the line we're returning.
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)
