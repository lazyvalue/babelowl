{-# LANGUAGE OverloadedStrings #-}
module Twiml where

import BabelTypes

import Control.Monad

import qualified Data.Map as M

import qualified Data.Text as T
import Text.XML 

mkElem :: T.Text -> [(T.Text,T.Text)] -> [Element] -> Element
mkElem name attrs subElems =
  Element (Name name Nothing Nothing) M.empty (map NodeElement subElems)

mkElemS :: T.Text -> [Element] -> Element
mkElemS name subElems = mkElem name [] subElems

--mkSuccessTwiml :: Lang -> T.Text -> T.Text
mkSuccessTwiml lang body =
  --documentRoot $ elementName (nameLocalName "twiml")
  Element (Name "twiml" Nothing Nothing) M.empty []
