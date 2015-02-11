{-# LANGUAGE OverloadedStrings #-}
module TwilioApi (
  callTwilioVoice
) where

import BabelTypes
import BabelConfig
import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Conduit

import Control.Monad.Catch
import Control.Monad.Trans.Resource (runResourceT)

twilioVoiceQuery :: TwilioConfig -> T.Text
twilioVoiceQuery creds =
  T.concat [ "https://api.twilio.com/2010-04-01/Accounts/", twilioAccountSid creds, 
    "/Calls.js"]

mkRequest :: MonadThrow m => TwilioConfig -> T.Text -> T.Text -> m Request
mkRequest creds insid to = do
  blankRequest <- parseUrl $ T.unpack $ twilioVoiceQuery creds
  let user = encodeUtf8 $ twilioAccountSid creds
  let pass = encodeUtf8 $ twilioAuthToken creds
  let authRequest = applyBasicAuth user pass blankRequest
  let callbackUrl = encodeUtf8 $ T.concat [ twilioCallCallback creds, insid ]
  let toNum = encodeUtf8 $ to
  let bodyContent = [ ("From", encodeUtf8 $ twilioPhoneNumber creds), ("To", toNum), ("Url", callbackUrl)  ]
  let bodiedRequest = urlEncodedBody bodyContent authRequest
  return bodiedRequest

callTwilioVoice :: TwilioConfig -> Manager -> T.Text -> T.Text -> IO ()
callTwilioVoice credentials manager insid to = runResourceT $ do
  request <- mkRequest credentials insid to
  httpLbs request manager
  return ()
