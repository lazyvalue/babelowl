{-# LANGUAGE OverloadedStrings #-}
module Twiml (
  mkSuccessTwiml
  , mkFailureTwiml
  , mkCallResponseTwiml
  , mkCallFail
) where

import BabelTypes
import BabelLangs
import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.XML 

type AttrLst = [(T.Text, T.Text)]

mkAttrs :: AttrLst -> M.Map Name T.Text
mkAttrs inAttrs =
  let tpler (attrN, attrV) = ((Name attrN Nothing Nothing), attrV)
  in M.fromList (map tpler inAttrs)

mkAttrElem :: T.Text -> AttrLst -> [Element] -> Element
mkAttrElem name attrLst subElems =
  Element (Name name Nothing Nothing) (mkAttrs attrLst) (map NodeElement subElems)

mkElem :: T.Text -> [Element] -> Element
mkElem name subElems = mkAttrElem name [] subElems

mkAttrTextElem :: T.Text -> AttrLst -> T.Text -> Element
mkAttrTextElem name attrs body =
  Element (Name name Nothing Nothing) (mkAttrs attrs) [(NodeContent body)]

mkTextElem :: T.Text -> T.Text -> Element
mkTextElem name body =
  mkAttrTextElem name [] body

mkDumbDocText :: Element -> TL.Text
mkDumbDocText docRoot =
  renderText def $ Document pro docRoot []
    where pro = Prologue [] Nothing []

mkSuccessTwimlElem :: T.Text -> Element
mkSuccessTwimlElem body =
  mkElem "Response" [
    mkTextElem "Message" body
  ]

mkFailureTwiml :: TL.Text
mkFailureTwiml = 
  mkDumbDocText (mkElem "Response" [mkTextElem "Message" "I have no idea"])

mkSuccessTwiml :: Lang -> T.Text -> TL.Text
mkSuccessTwiml lang body = 
  mkDumbDocText (mkSuccessTwimlElem body)

mkSay :: T.Text -> T.Text -> Element
mkSay lang body = undefined

mkCallResponseTwiml :: T.Text -> T.Text -> TL.Text
mkCallResponseTwiml lang body = 
  mkDumbDocText $ mkElem "Response" [
    mkAttrTextElem "Say" [("voice", "woman"), ("language", lang)] body
  ]

mkCallFail :: TL.Text
mkCallFail = mkDumbDocText (mkElem "Response" [mkElem "Hangup" []])
