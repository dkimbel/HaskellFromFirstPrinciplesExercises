{-# LANGUAGE OverloadedStrings #-}

module MaybeExample where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Web.Scotty
import Data.Monoid (mconcat)

param' :: Parsable a
       => Text -> MaybeT ActionM a
param' k = MaybeT $
             rescue (Just <$> param k)
                    (const (return Nothing))

type Reco = (Integer, Integer, Integer, Integer)

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    reco <- runMaybeT $ do
      a <- param' "1"
      b <- param' "2"
      c <- param' "3"
      d <- param' "4"
      (lift . lift) $ print b
      return ((a, b, c, d) :: Reco)
    liftIO $ print reco
    html $
      mconcat ["<h1>Scotty, ",
               beam,
               " me up!</h1>"]
