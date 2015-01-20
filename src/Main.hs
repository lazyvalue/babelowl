{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

-- Local imports
import Twiml
import BabelTypes

import Network.HTTP.Conduit (parseUrl, newManager, httpLbs, RequestBody(..), Response(..), HttpException(..), Request(..), Manager, simpleHttp, withManager)

import Network.HTTP.Client.TLS (tlsManagerSettings)

import Data.Aeson
import Data.Conduit 
import Data.Either
import Data.Functor
import Data.Maybe (fromJust)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Exception.Base
import Control.Monad.Trans.Resource (runResourceT)

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

getGoogleTranslation :: Manager -> GoogleKey -> Lang -> T.Text -> IO GoogleTranslateResult
getGoogleTranslation man gKey lang body = runResourceT $ do
  req <- parseUrl $ T.unpack $ googleTranslateQuery gKey lang body
  response <- (httpLbs req man)
  let mResult = decode $ responseBody response
  case mResult of
    Just transResult -> return transResult
    Nothing -> 
      let x = putStrLn "Fucked" -- TBD better error handling
      in liftIO mzero

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
  httpManager <- newManager tlsManagerSettings
  let translateF = getGoogleTranslation httpManager googleKey 
  scotty 3000 $ do
    post "/translate" $ webTranslateAction translateF
  --trans <- getGoogleTranslation googleKey (Lang "french" "fr") "i love the french"
  --putStrLn $ show trans
