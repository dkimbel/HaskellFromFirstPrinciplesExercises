{-# LANGUAGE OverloadedStrings #-}

module EitherExample where

import Control.Monad.IO.Class
import Data.Text.Lazy (Text)
import Web.Scotty
import Data.Monoid (mconcat)

param' :: Parsable a
       => Text -> ActionM (Either String a)
param' k = rescue (Right <$> param k)
                  (const 
                    (return
                      (Left $ "The key: " 
                              ++ show k
                              ++ " was missing!")))

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    a <- param' "1"
    let a' = either (const 0) id a
    liftIO $ print (a :: Either String Int)
    liftIO $ print (a' :: Int)
    html $
      mconcat ["<h1>Scotty, ",
               beam,
               " me up!</h1>"]
