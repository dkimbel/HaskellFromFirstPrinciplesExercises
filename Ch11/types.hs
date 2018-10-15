{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

type Cows = Int

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany (Int, String) where
    tooMany (n, _) = tooMany n

--instance TooMany (Int, Int) where
--    tooMany (x, y) = tooMany (x + y)

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, y) = tooMany x || tooMany y

--instance TooMany Goats where
--    tooMany (Goats n) = n > 43

-- this would break compilation; typeclass instances
-- are not allowed for type synonyms, even if there's
-- no pre-existing instance for the synonymous type
--instance TooMany Cows where
--    tooMany c = c > 44
