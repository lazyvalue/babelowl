{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import qualified Data.Text as T

import Network.HTTP.Conduit (parseUrl, newManager, httpLbs, RequestBody(..), Response(..), HttpException(..), Request(..), Manager, simpleHttp, withManager)
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header (ResponseHeaders)

import Network.HTTP.Client.TLS (tlsManagerSettings)

import Data.Functor
import Data.Conduit 
import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Exception.Base
import Control.Monad.Trans.Resource (runResourceT)

import qualified Data.Map.Strict as Map

import Data.Aeson

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B

-- [ Type definitions 
data Lang = Lang {
  lang_Name :: T.Text
  , lang_Code :: T.Text
} deriving Show

data TwilioCredentials = TwilioCredentials {
  twilio_accountSid :: T.Text
  , twilio_authToken :: T.Text
} deriving Show

newtype GoogleKey = GoogleKey { getGoogleKey :: T.Text }

data GoogleTranslation = GoogleTranslation {
  gtr_DetectedSourceLanguage :: T.Text
  , gtr_Translation :: T.Text
} deriving Show

newtype GoogleTranslateResult = GoogleTranslateResult { getGoogleTranslations :: [GoogleTranslation] }

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

-- https://www.google.com/language/translate/v2?q=i+love+the+french&target=fr&key=AIzaSyBjzHewaAbupYYNJJLlNlAplSPsoyuibxw

-- [ Request building
googleTranslateQuery :: GoogleKey -> Lang -> T.Text -> T.Text
googleTranslateQuery key lang body =
  T.concat [
    "https://www.google.com/language/translate/v2?key=" , getGoogleKey key
    , "&target=", lang_Code lang
    , "&q=", body
  ]

getTranslation :: GoogleKey -> Lang -> T.Text -> IO GoogleTranslateResult
getTranslation gKey lang body = runResourceT $ do
  req <- parseUrl $ T.unpack $ googleTranslateQuery gKey lang body
  man <- liftIO $ newManager tlsManagerSettings
  response <- (httpLbs req man)
  let mResult = decode $ responseBody response
  case mResult of
    Just transResult -> return transResult
    Nothing -> 
      let x = putStrLn "Fucked" -- TBD better error handling
      in liftIO mzero


doRequest urlStr = do
  theBody <- runResourceT $ do
    manager <- liftIO $ newManager tlsManagerSettings
    req <- liftIO $ parseUrl urlStr
    httpLbs req manager
  LB.putStrLn $ responseBody theBody

doRequest2 urlStr = do
  someBody <- simpleHttp urlStr
  LB.putStrLn $ someBody

-- [ Main function entry point
--main = scotty 3000 $ do
--  get "/" $ do
--    html "Hello World!"
main :: IO ()
main = do
  doRequest2 "https://google.com"
