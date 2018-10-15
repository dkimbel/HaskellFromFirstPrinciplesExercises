{-# LANGUAGE InstanceSigs #-}

import Control.Monad (join)

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure a = MaybeT $ pure $ pure a

  (<*>) :: MaybeT m (a -> b)
        -> MaybeT m a
        -> MaybeT m b
  MaybeT mf <*> MaybeT ma = 
--    MaybeT $ (pure (<*>)) <*> mf <*> ma
-- NOTE: as the book points out, a cleaner and simpler 
-- definition is the following:
    MaybeT $ (<*>) <$> mf <*> ma
-- Another possibility taken from other book examples:
--    MaybeT $ ((<*>) . (fmap (<*>))) mf ma

instance Monad m => Monad (MaybeT m) where
  return = pure

  -- This definition is from the book; I wasn't able to
  -- get it myself
  (>>=) :: MaybeT m a
        -> (a -> MaybeT m b)
        -> MaybeT m b
  MaybeT ma >>= f = 
    MaybeT $ do
      v <- ma
      case v of 
        Nothing -> return Nothing
        Just y -> runMaybeT (f y)
  -- Desugared from the book's definition  
--  MaybeT ma >>= f = 
--    MaybeT $
--      ma >>= \v ->
--        case v of 
--          Nothing -> return Nothing
--          Just y -> runMaybeT (f y)
    
--    MaybeT $ join $ fmap runMaybeT $ (fmap . fmap) f ma
