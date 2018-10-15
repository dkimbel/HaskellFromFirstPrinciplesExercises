module OurExceptions where

import Control.Exception

data EATD =
    NotEven Int
  | NotDivThree Int
  deriving (Eq, Show)

instance Exception EATD

--data NotDivThree =
--  NotDivThree Int
--  deriving (Eq, Show)
--
--instance Exception NotDivThree
--
--data NotEven =
--  NotEven Int
--  deriving (Eq, Show)
--
--instance Exception NotEven

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
  | rem i 3 /= 0 = throwIO (NotDivThree i)
  | odd i = throwIO (NotEven i)
  | otherwise = return i

handleEATD :: EATD
           -> IO Int
handleEATD err = do
  print $ "Caught " ++ show err ++ ", returning 0"
  return 0

-- USAGE
-- `handle handleEATD (evenAndThreeDiv 1)`
-- or
-- `catch (evenAndThreeDiv 1) handleEATD`

--catchNotDivThree :: IO Int
--                 -> (NotDivThree -> IO Int)
--                 -> IO Int
--catchNotDivThree = catch
--
--catchNotEven :: IO Int
--             -> (NotEven -> IO Int)
--             -> IO Int
--catchNotEven = catch

--catchBoth :: IO Int
--          -> IO Int
--catchBoth ioInt =
--  catches ioInt
--  [ Handler
--      (\(NotEven _) -> return maxBound)
--  , Handler
--      (\(NotDivThree _) -> return minBound)
--  ]

