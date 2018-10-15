{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Marshall where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import Data.Text (Text)
import Text.RawString.QQ

sectionJson :: LBS.ByteString
sectionJson = [r|
{ "section": {"host": "wikipedia.org"},
  "whatisit": {"red": "intoothandclaw"}
}
|]

data TestData = 
  TestData {
    section :: Host
  , what :: Color
  } deriving (Eq, Show)

newtype Host =
  Host String
  deriving (Eq, Show)

type Annotation = String

data Color =
    Red Annotation
  | Blue Annotation
  | Yellow Annotation
  deriving (Eq, Show)

instance FromJSON TestData where
  parseJSON (Object v) =
    TestData <$> v .: "section"
             <*> v .: "whatisit"
  parseJSON _ =
    fail "Expected an object for TestData"

instance FromJSON Host where
  parseJSON (Object v) =
    Host <$> v .: "host"
  parseJSON _ =
    fail "Expected an object for Host"

instance FromJSON Color where
  parseJSON (Object v) =
        (Red <$> v .: "red")
    <|> (Blue <$> v .: "blue")
    <|> (Yellow <$> v .: "yellow")
  parseJSON _ =
    fail "Expected an object for Color"

data NumberOrString =
    Numba Integer
  | Stringy Text
  deriving (Eq, Show)

instance FromJSON NumberOrString where
  parseJSON (Number n) =
    case floatingOrInteger n of
      (Left _) -> fail "Must be integral number"
      (Right i) -> return $ Numba i
  parseJSON (String s) = return $ Stringy s
  parseJSON _ = fail "Expected number or string for \
                     \NumberOrString"

dec :: LBS.ByteString
    -> Maybe NumberOrString
dec = decode

eitherDec :: LBS.ByteString
          -> Either String NumberOrString
eitherDec = eitherDecode

main :: IO ()
main = do
  print $ dec "blah"
  print $ eitherDec "blah"
--  let d :: Maybe TestData
--      d = decode sectionJson
--  print d
