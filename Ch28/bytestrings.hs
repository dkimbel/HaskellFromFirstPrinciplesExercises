{-# LANGUAGE OverloadedStrings #-}

module BS where

import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.GZip as GZip

input :: BL.ByteString
input = "123"

compressed :: BL.ByteString
compressed = GZip.compress input

main :: IO ()
main = do
  TIO.putStrLn $ TE.decodeUtf8 (s input)
  -- This second call is intended to fail; it will contain
  -- a byte that isn't recognizable as UTF-8 text
  TIO.putStrLn $ TE.decodeUtf8 (s compressed)
  where s = BL.toStrict
