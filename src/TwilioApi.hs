{-# LANGUAGE OverloadedStrings #-}
module TwilioApi (
  callTwilioVoice
) where

import BabelTypes
import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Conduit

import Control.Monad.Catch
import Control.Monad.Trans.Resource (runResourceT)

twilioVoiceQuery :: TwilioCredentials -> T.Text
twilioVoiceQuery creds =
  T.concat [ "https://api.twilio.com/2010-04-01/Accounts/", twilio_accountSid creds, 
    "/Calls.js"]

mkRequest :: MonadThrow m => TwilioCredentials -> T.Text -> T.Text -> m Request
mkRequest creds insid to = do
  blankRequest <- parseUrl $ T.unpack $ twilioVoiceQuery creds
  let user = encodeUtf8 $ twilio_accountSid creds
  let pass = encodeUtf8 $ twilio_authToken creds
  let authRequest = applyBasicAuth user pass blankRequest
  let urlBlah = [ "http://babelowl.thereceptor.net?/call?InSid=" , insid ]
  let callbackUrl = encodeUtf8 $ T.concat urlBlah
  let toNum = encodeUtf8 $ to
  let bodyContent = [ ("From", "+19253266177"), ("To", toNum), ("url", callbackUrl)  ]
  let bodiedRequest = urlEncodedBody bodyContent authRequest
  return bodiedRequest

callTwilioVoice :: TwilioCredentials -> Manager -> T.Text -> T.Text -> IO ()
callTwilioVoice credentials manager insid to = runResourceT $ do
  request <- mkRequest credentials insid to
  httpLbs request manager
  return ()
