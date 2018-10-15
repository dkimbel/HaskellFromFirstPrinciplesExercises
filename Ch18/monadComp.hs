import Control.Monad ((>=>))

mcomp :: Monad m =>
         (b -> m c)
      -> (a -> m b)
      ->  a -> m c
-- book's naive example that doesn't work
--mcomp f g a = f (g a)
-- my implementation that works
--mcomp f g a = do
--  b <- g a
--  c <- f b
--  return c
-- my implementation that follows the book's lead
-- by using fmap and join
--mcomp f g a = join $ fmap f (g a)
-- the book's own super-terse implementation using bind
mcomp f g a = g a >>= f

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

getAge' :: String -> IO Int
getAge' s = sayHi s >>= readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you? "

askForAge' :: IO Int
askForAge' = getAge' "Hello! How old are you? "
