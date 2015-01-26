{-# LANGUAGE OverloadedStrings #-}
module Twiml (
  mkSuccessTwiml
  , mkFailureTwiml
) where

import BabelTypes
import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.XML 

mkAttrElem :: T.Text -> [(T.Text,T.Text)] -> [Element] -> Element
mkAttrElem name attrLst subElems =
  Element (Name name Nothing Nothing) attrM (map NodeElement subElems)
  where tpler (attrN, attrV) = ((Name attrN Nothing Nothing), attrV)
        attrM = M.fromList (map tpler attrLst)

mkElem :: T.Text -> [Element] -> Element
mkElem name subElems = mkAttrElem name [] subElems

mkTextElem :: T.Text -> T.Text -> Element
mkTextElem name body =
  Element (Name name Nothing Nothing) M.empty [(NodeContent body)]

mkDumbDocText :: Element -> TL.Text
mkDumbDocText docRoot =
  renderText def $ Document pro docRoot []
    where pro = Prologue [] Nothing []

mkSuccessTwimlElem :: Lang -> T.Text -> Element
mkSuccessTwimlElem lang body =
  mkElem "Response" [
    mkTextElem "Message" body
  ]

mkFailureTwiml :: TL.Text
mkFailureTwiml = 
  mkDumbDocText (mkElem "Response" [mkTextElem "Message" "I have no idea"])

mkSuccessTwiml :: Lang -> T.Text -> TL.Text
mkSuccessTwiml lang body = 
  mkDumbDocText (mkSuccessTwimlElem lang body)
