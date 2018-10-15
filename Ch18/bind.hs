import Control.Monad (join)

-- As the book noted, this will be a flipped version
-- of `>>=`
-- The book says to implement this in terms of fmap
-- and join
bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma = join $ fmap f ma
