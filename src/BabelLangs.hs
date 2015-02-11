{-# LANGUAGE OverloadedStrings #-}
module BabelLangs where (
  Lang (..)
  , langName
  , langCode
)

import qualified Data.Text as T

data Lang = German | Italian | French | Spanish

langName :: Lang -> T.Text
langName German = "german"
langName Italian = "italian"
langName French = "french"
langName Spanish = "spanish"

langCode :: Lang -> T.Text
langCode German = "de"
langCode Italian = "it"
langCode French = "fr"
langCode Spanish = "es"

langList :: [Lang]
langList = [German, Italian, French, Spanish]

langNameMap :: T.Text -> Maybe Lang
langNameMap = Map.fromList $ map (\x -> (langName x, x)) langList

getLangByName :: T.Text -> Maybe Lang
getLangByName inName = Map.lookup (T.toLower inName) langNameMap
