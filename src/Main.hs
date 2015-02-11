{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

-- Local imports
import Twiml
import BabelLangs
import BabelTypes
import BabelConfig
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

-- handy function declarations
type GetTranslation = Lang -> T.Text -> IO GoogleTranslateResult
type Storer = T.Text -> T.Text -> IO ()
type StoreGetter = T.Text -> IO (Maybe T.Text)
type Caller = T.Text -> T.Text -> IO ()

-- [ Request building
googleTranslateQuery :: T.Text -> Lang -> T.Text -> T.Text
googleTranslateQuery key lang body =
  T.concat [
    "https://www.googleapis.com/language/translate/v2?key=" , key
    , "&target=", langCode lang
    , "&q=", body
  ]

getGoogleTranslation :: Manager -> T.Text -> Lang -> T.Text -> IO GoogleTranslateResult
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
  fromNum <- param "From"
  let parseResult = parseTextRequest $ TL.toStrict msgBody
  sometext <- case parseResult of
    Left problem -> 
      return $ mkFailureTwiml
    Right (lang, text) -> do 
      liftIO $ caller msgSid fromNum
      liftIO (do 
        translationResult <- getTransF lang text
        let transText = gtr_Translation $ head (getGoogleTranslations translationResult)
        liftIO $ storer msgSid (T.concat [langCode lang, langMsgSep, transText])
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
  config <- parseConfigFromFile

  -- set up http client
  httpManager <- newManager tlsManagerSettings

  -- set up translate
  let translateF = getGoogleTranslation httpManager (googleKey config)

  -- set up caller
  let caller = callTwilioVoice (twilioConfig config) httpManager

  -- setup redis
  let redisConnectInfo = R.defaultConnectInfo { 
    R.connectHost = T.unpack (redisHost config)
  }

  redisConnection <- R.connect redisConnectInfo
  let getter = getDb redisConnection
  let storer = putDb redisConnection
    
  -- setup web server
  scotty 3000 $ do
    post "/translate" $ webTranslateAction translateF storer caller
    post "/call" $ webSpeakAction getter
    get "/" $ file "index.html"
