{-# LANGUAGE OverloadedStrings #-}
module BabelConfig (
  BabelConfig (..)
  , TwilioConfig (..)
  , parseConfigFromFile
) where

import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import Control.Applicative
import Data.Functor

data TwilioConfig = TwilioConfig {
  twilioAccountSid :: T.Text
  , twilioAuthToken :: T.Text
  , twilioPhoneNumber :: T.Text
  , twilioCallCallback :: T.Text
}

data BabelConfig = BabelConfig {
  twilioConfig :: TwilioConfig
  , googleKey :: T.Text
  , redisHost :: T.Text
  , redisPort :: Int
}

parseConfig :: T.Text -> Maybe BabelConfig
parseConfig confBuf =
  let tuplify [x,y] = (x,y)
      configMap = Map.fromList $ map (tuplify . (T.split (== '='))) $ T.lines confBuf
      lku = (flip Map.lookup) configMap
      twilConfig = TwilioConfig <$> lku "twilioAccountSid"
        <*> lku "twilioAuthToken" 
        <*> lku "twilioPhoneNumber"
        -- <*> lku "twilioCallCallback"
	<*> Just "http://babelowl.thereceptor.net:3000/call?InSid="

  in  BabelConfig <$> twilConfig
        <*> lku "googleApiKey"
        <*> lku "redisHost"
        <*> Just 6397

parseConfigFromFile :: IO BabelConfig
parseConfigFromFile = do
  buf <- B.readFile "babelOwlConfig"
  let processed = TE.decodeUtf8 buf
  return $ fromJust (parseConfig processed)
