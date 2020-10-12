{-# LANGUAGE TemplateHaskell #-}
module Data.Patent.Number where
import Prelude

import Control.Lens.TH
import Control.Lens

data PatentNumber = PatentNumber {_country :: !Text, _serial :: !Text, _kind :: !(Maybe Text)} deriving (Eq, Show, Generic)
makeFieldsNoPrefix ''PatentNumber

toEPODOC :: PatentNumber -> Text
toEPODOC pn = pn ^. country <> pn ^. serial <> fromMaybe "" (pn ^. kind)

mkPatentNumber :: Text -> Text -> Maybe Text -> PatentNumber
mkPatentNumber _country _serial _kind = PatentNumber {..}

mkPatentNumber' :: Text -> Text -> Text -> PatentNumber
mkPatentNumber' country serial kind' = mkPatentNumber country serial (Just kind')
