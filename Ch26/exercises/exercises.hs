module Exercises where

--import Control.Monad.Trans.Class
--import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
--import Control.Monad.Trans.Maybe
--import Control.Monad.IO.Class
import Data.Functor.Identity

rDec :: Num a => Reader a a
rDec = ReaderT $ \r -> Identity $ r - 1

-- Totally pointfree version (not evey using a lambda with
-- a named argument). This would have been easier if `(-1)`
-- were a sectioned version of subtraction rather than a
-- negative number.
-- Note that we can use `<$>` in place of `.` if desired;
-- after all, the functor of functions IS function composition.
rDec' :: Num a => Reader a a
rDec' = ReaderT $ Identity . (flip (-) 1)

rShow :: Show a
      => ReaderT a Identity String
rShow = ReaderT $ \r -> Identity (show r)

-- Pointfree version
rShow' :: Show a
       => ReaderT a Identity String
rShow' = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a)
             => ReaderT a IO a
--rPrintAndInc = undefined
rPrintAndInc = ReaderT $ \r -> do
                 putStrLn $ "Hi: " ++ show r
                 return $ r + 1

sPrintIncAccum :: (Num a, Show a)
               => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
                   putStrLn $ "Hi: " ++ show s
                   return (show s, s + 1)

