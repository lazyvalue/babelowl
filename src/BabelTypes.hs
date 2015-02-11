{-# LANGUAGE OverloadedStrings #-}
module BabelTypes (
  ParseException (..)
  , TwilioCredentials (..)
  , GoogleKey (..)
  , GoogleTranslation (..)
  , GoogleTranslateResult (..)
  , RedisConfig (..)
) where

import Control.Applicative

import Data.Aeson
import Data.Functor
import qualified Data.Text as T

import Control.Monad

data ParseException = BadFormat | UnknownLanguage deriving Show

data TwilioCredentials = TwilioCredentials {
  twilio_accountSid :: T.Text
  , twilio_authToken :: T.Text
} deriving Show

newtype GoogleKey = GoogleKey { getGoogleKey :: T.Text } deriving Show

data RedisConfig = RedisConfig {
  redisHost :: T.Text
  , redisPort :: Int
}

data GoogleTranslation = GoogleTranslation {
  gtr_DetectedSourceLanguage :: T.Text
  , gtr_Translation :: T.Text
} deriving Show

newtype GoogleTranslateResult = GoogleTranslateResult { getGoogleTranslations :: [GoogleTranslation] } deriving Show

instance FromJSON GoogleTranslation where
  parseJSON (Object v) =
    GoogleTranslation <$>
      v .: "detectedSourceLanguage" <*>
      v .: "translatedText"
  parseJSON _ = mzero

instance FromJSON GoogleTranslateResult where
  parseJSON (Object v) = do
    dataNode <- v .: "data"
    GoogleTranslateResult <$> dataNode .: "translations"
  parseJSON _ = mzero
