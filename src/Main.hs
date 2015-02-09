{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

-- Local imports
import Twiml
import BabelTypes
import TwilioApi

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

import qualified Database.Redis as R

-- [ Language maps and tools

langList = [ Lang "french" "fr", Lang "german" "de" ]

langCodeMap = Map.fromList $ map (\l -> (lang_Code l, l)) langList

langNameMap = Map.fromList $ map (\l -> (lang_Name l, l)) langList

getLangByName :: T.Text -> Maybe Lang
getLangByName inName = Map.lookup (T.toLower inName) langNameMap

-- handy function declarations
type GetTranslation = Lang -> T.Text -> IO GoogleTranslateResult
type Storer = T.Text -> T.Text -> IO ()
type StoreGetter = T.Text -> IO (Maybe T.Text)
type Caller = T.Text -> IO ()

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

webTranslateAction :: GetTranslation -> Storer -> Caller -> ActionM ()
webTranslateAction getTransF storer caller = do
  msgBody <- param "Body"
  msgSid <- param "MessageSid"
  let parseResult = parseTextRequest $ TL.toStrict msgBody
  sometext <- case parseResult of
    Left problem -> 
      return $ mkFailureTwiml
    Right (lang, text) -> do 
      liftIO $ storer msgSid (T.concat [lang_Code lang, langMsgSep, text])
      liftIO $ caller msgSid
      liftIO (do 
        translationResult <- getTransF lang text
        let transText = gtr_Translation $ head (getGoogleTranslations translationResult)
        return $ mkSuccessTwiml lang transText)
  html sometext

webSpeakAction :: StoreGetter -> ActionM ()
webSpeakAction getter = do
  inSid <- param "InSid"
  rawRecordMaybe <- liftIO $ getter $ TL.toStrict inSid
  case rawRecordMaybe of
    Nothing -> html mkCallFail
    Just rawRecord ->
      let (lang,body) = tuplify $ T.split (== (T.head langMsgSep)) rawRecord
      in html $ mkCallResponseTwiml lang body

parseConfig :: T.Text -> Maybe (GoogleKey, TwilioCredentials, RedisConfig)
parseConfig confBuf =
  let configMap = Map.fromList $ map (tuplify . (T.split (== '='))) $ T.lines confBuf
      lookup = (flip Map.lookup) configMap
      googleKey = GoogleKey <$> lookup "googleApiKey"
      twiKey = TwilioCredentials <$> lookup "twilioAccountSid" <*> lookup "twilioAuthToken"
      redisConfig = RedisConfig <$> lookup "redisHost" <*> Just 6379
  in (,,) <$> googleKey <*> twiKey <*> redisConfig


tuplify [x,y] = (x,y)

-- [ DB operations
langMsgSep :: T.Text
langMsgSep = "|"

putDb :: R.Connection -> T.Text -> T.Text -> IO ()
putDb conn key val = R.runRedis conn $ do
  let keyName = TE.encodeUtf8 key
  R.set keyName (TE.encodeUtf8 val)
  R.expire keyName 3600
  return ()

getDb :: R.Connection -> T.Text -> IO (Maybe T.Text)
getDb conn key = R.runRedis conn $ do
  ret <- R.get $ TE.encodeUtf8 key
  return $ case ret of
    Left _ -> Nothing
    Right (Just v) -> Just $ TE.decodeUtf8 v
    Right _ -> Nothing

main :: IO ()
main = do
  -- parse config
  configBuf <- fmap TE.decodeUtf8 $ B.readFile "babelOwlConfig"
  let (googleKey,twilioCredentials, redisConfig) = fromJust $ parseConfig configBuf

  -- set up http client
  httpManager <- newManager tlsManagerSettings

  -- set up translate
  let translateF = getGoogleTranslation httpManager googleKey 

  -- set up caller
  let caller = callTwilioVoice twilioCredentials httpManager

  -- setup redis
  let redisConnectInfo = R.defaultConnectInfo { 
    R.connectHost = T.unpack (redisHost redisConfig)
  }

  redisConnection <- R.connect redisConnectInfo
  let getter = getDb redisConnection
  let storer = putDb redisConnection
    
  -- setup web server
  scotty 3000 $ do
    post "/translate" $ webTranslateAction translateF storer caller
    get "/call" $ webSpeakAction getter
