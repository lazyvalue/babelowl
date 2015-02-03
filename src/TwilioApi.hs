{-# LANGUAGE OverloadedStrings #-}
module TwilioApi where

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

mkRequest :: MonadThrow m => TwilioCredentials -> m Request
mkRequest creds = do
  blankRequest <- parseUrl $ T.unpack $ twilioVoiceQuery creds
  let user = encodeUtf8 $ twilio_accountSid creds
  let pass = encodeUtf8 $ twilio_authToken creds
  let authRequest = applyBasicAuth user pass blankRequest
  let bodyContent = [ ("From", "+19253266177"), ("url", "http://babelowl.thereceptor.net") ]
  let bodiedRequest = urlEncodedBody bodyContent authRequest
  return bodiedRequest

callTwilioVoice :: TwilioCredentials -> Manager -> IO ()
callTwilioVoice credentials manager = runResourceT $ do
  request <- mkRequest credentials
  httpLbs request manager
  return ()
