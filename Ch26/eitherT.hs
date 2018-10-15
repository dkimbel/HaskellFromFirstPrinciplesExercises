{-# LANGUAGE InstanceSigs #-}

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b)
       -> EitherT e m a
       -> EitherT e m b
  fmap f (EitherT me) = 
    EitherT $ (fmap . fmap) f me

instance Applicative m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure a = EitherT $ pure $ pure a

  (<*>) :: EitherT e m (a -> b)
        -> EitherT e m a
        -> EitherT e m b
  EitherT meaB <*> EitherT mea =
    EitherT $ (<*>) <$> meaB <*> mea

instance Monad m => Monad (EitherT e m) where
  return :: a -> EitherT e m a
  return = pure

  (>>=) :: EitherT e m a
        -> (a -> EitherT e m b)
        -> EitherT e m b
  EitherT mea >>= f =
    EitherT $ mea >>= \ev ->
      case ev of
        Left l  -> return $ Left l
        Right r -> runEitherT $ f r

swapEitherT :: (Functor m)
            => EitherT e m a
            -> EitherT a m e
swapEitherT (EitherT mea) =
  EitherT $ swapEither <$> mea

swapEither :: Either e a -> Either a e
swapEither (Left l) = Right l
swapEither (Right r) = Left r

eitherT :: Monad m
        => (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT mea) =
  mea >>= \ev ->
    case ev of
      Left a  -> f a
      Right b -> g b

