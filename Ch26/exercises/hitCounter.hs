{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans
import Network.Wai.Internal

data Config =
  Config {
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty =
  ScottyT Text (ReaderT Config IO)

--type Handler =
--  ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m = (newMap, newVal)
  where
    newMap = M.insertWith (+) k 1 m
    newVal = fromMaybe 0 $ M.lookup k newMap

app :: Scotty ()
app =
  get "/:key" $ do
    pre <- lift $ asks prefix
    countsRef <- lift $ asks counts
    countsMap <- liftIO $ readIORef countsRef
    unprefixed <- param "key"
    let key' = mappend pre unprefixed 
        (newMap, newInteger) = bumpBoomp key' countsMap
    liftIO $ writeIORef countsRef newMap
    html $
      mconcat [ "<h1>Success! Count was: "
              , TL.pack $ show newInteger
              , "</h1>"
              ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      -- I had no idea how to write this line; it's inspired
      -- by the Scotty GitHub page's Reader example at
      -- https://github.com/scotty-web/scotty/blob/master/examples/reader.hs
      runR :: ReaderT Config IO Response -> IO Response
      runR m = runReaderT m config
  scottyT 3000 runR app
