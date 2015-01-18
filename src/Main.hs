{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Translate

import Network.HTTP.Conduit (parseUrl, newManager, httpLbs, RequestBody(..), Response(..), HttpException(..), Request(..), Manager, simpleHttp, withManager)

import Network.HTTP.Client.TLS (tlsManagerSettings)

import Data.Functor
import Data.Maybe (fromJust)
import Data.Conduit 
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Exception.Base
import Control.Monad.Trans.Resource (runResourceT)

import qualified Data.Map.Strict as Map

import Data.Aeson
import Data.Either

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B

-- [ Type definitions 
data Lang = Lang {
  lang_Name :: T.Text
  , lang_Code :: T.Text
} deriving Show

data ParseException = BadFormat | UnknownLanguage deriving Show

data TwilioCredentials = TwilioCredentials {
  twilio_accountSid :: T.Text
  , twilio_authToken :: T.Text
} deriving Show

newtype GoogleKey = GoogleKey { getGoogleKey :: T.Text } deriving Show

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

-- [ Language maps and tools

langList = [ Lang "french" "fr", Lang "german" "de" ]

langCodeMap = Map.fromList $ map (\l -> (lang_Code l, l)) langList

langNameMap = Map.fromList $ map (\l -> (lang_Name l, l)) langList

getLangByName :: T.Text -> Maybe Lang
getLangByName inName = Map.lookup (T.toLower inName) langNameMap

type GetTranslation = Lang -> T.Text -> IO GoogleTranslateResult

-- [ Request building
googleTranslateQuery :: GoogleKey -> Lang -> T.Text -> T.Text
googleTranslateQuery key lang body =
  T.concat [
    "https://www.googleapis.com/language/translate/v2?key=" , getGoogleKey key
    , "&target=", lang_Code lang
    , "&q=", body
  ]

getGoogleTranslation :: GoogleKey -> Lang -> T.Text -> IO GoogleTranslateResult
getGoogleTranslation gKey lang body = runResourceT $ do
  req <- parseUrl $ T.unpack $ googleTranslateQuery gKey lang body
  man <- liftIO $ newManager tlsManagerSettings
  response <- (httpLbs req man)
  let mResult = decode $ responseBody response
  case mResult of
    Just transResult -> return transResult
    Nothing -> 
      let x = putStrLn "Fucked" -- TBD better error handling
      in liftIO mzero

tryMe :: T.Text -> T.Text
tryMe v = T.concat ["this is silly ", v]

parseTextRequest :: T.Text -> Either ParseException (Lang, T.Text)
parseTextRequest input = do
  let someWords = T.words input
  let wordLength = length someWords 
  if (wordLength < 3) then Left BadFormat else Right ()
  let toTranslate = T.unwords $ take (wordLength - 2) someWords
  let langText = head (reverse someWords)
  case (getLangByName langText) of
    Just lang -> return (lang, toTranslate)
    Nothing -> Left UnknownLanguage

-- [ Main function entry point
-- main2 = scotty 3000 $ do
--   get "/" $ do
--     x <- param "lar"
--     html $ TL.pack $ show (tryMe x)

webTranslateAction :: GetTranslation -> ActionM ()
webTranslateAction getTransF = do
  msgBody <- param "Body"
  let parseResult = parseTextRequest $ TL.toStrict msgBody
  sometext <- case parseResult of
    Left problem -> 
      return $ TL.pack $ show problem
    Right (lang, text) -> liftIO (do 
        translationResult <- getTransF lang text
        let transText = gtr_Translation $ head (getGoogleTranslations translationResult)
        return $ TL.fromStrict transText)
  html sometext

parseConfig :: T.Text -> Maybe (GoogleKey, TwilioCredentials)
parseConfig confBuf =
  let tuplify [x,y] = (x,y)
      configMap = Map.fromList $ map (tuplify . (T.split (== '='))) $ T.lines confBuf
      lookup = (flip Map.lookup) configMap
      googleKey = GoogleKey <$> lookup "googleApiKey"
      twiKey = TwilioCredentials <$> lookup "twilioAccountSid" <*> lookup "twilioAuthToken"
	in (\x y -> (x,y)) <$> googleKey <*> twiKey

main :: IO ()
main = do
  configBuf <- fmap TE.decodeUtf8 $ B.readFile "babelOwlConfig"
  let (googleKey,twilioCredentials) = fromJust $ parseConfig configBuf
  let translateF = getGoogleTranslation googleKey 
  --scotty 3000 $ do
  --  post "/translate" $ webTranslateAction translateF
  trans <- getGoogleTranslation googleKey (Lang "french" "fr") "i love the french"
  putStrLn $ show trans
