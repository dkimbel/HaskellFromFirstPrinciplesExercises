module OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

embedded :: MaybeT
            (ExceptT String
                     (ReaderT () IO))
            Int
embedded = return 1

maybeUnwrap :: ExceptT String
               (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO
                (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: ()
             -> IO (Either String
                           (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

-- enter `readerUnwrap ()` in ghci to get `Right (Just 1)`

-- the above is all from the book; the below is partially
-- mine, as an exercise from the book
embedded' :: MaybeT
             (ExceptT String
                      (ReaderT () IO))
             Int
embedded' = 
  MaybeT $ ExceptT $ ReaderT readerUnwrap

-- the book wanted us to actually do this in terms of 
-- `(const (Right (Just 1)))`, so I did
embedded'' :: MaybeT
              (ExceptT String
                       (ReaderT () IO))
              Int
embedded'' = 
  MaybeT $ ExceptT $ ReaderT $ \_ -> ioStarter
    where
      starter = const $ Right $ Just 1
      ioStarter = return (starter ())

{- NOTES
  Per the book, stacking monad transformers can be equivalent
  to function composition. Compare `readerUnwrap ()` to
  `(const . Right . Just $ 1) ()` -- their results are
  identical.

  According to the book, "base monad" usually refers to
  the structurally outermost monad. In the case of plain
  monads, that would mean the outermost monad in the 
  type signature. In the case of monad transformers, I'm not
  sure whether it would mean the outermost transformer in
  the type signature or whether transformers would be
  ignored in favor of their corresponding monads.
-}
