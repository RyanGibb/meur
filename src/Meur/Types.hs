{-# LANGUAGE OverloadedStrings #-}

module Meur.Types
  ( BibKind (..),
    CombinedItem (..),
    Output (..),
    FeedType (..),
    PhotoLocation (..),
  )
where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Hakyll (CopyFile, Item)
import Meur.Bib (Bib)

data BibKind = Paper | Talk
  deriving (Show, Eq)

data CombinedItem = PostItem (Item String) | BibItem BibKind Bib | PhotoItem (Item CopyFile)

data Output = HTML | MD
  deriving (Show, Eq)

data FeedType = XmlFeed | JsonFeed
  deriving (Show, Eq)

data PhotoLocation = PhotoLocation
  { displayName :: Text,
    addressMap :: HM.HashMap Text Text
  }
  deriving (Show)

instance FromJSON PhotoLocation where
  parseJSON = withObject "PhotoLocation" $ \v ->
    PhotoLocation
      <$> v .: "display_name"
      <*> v .: "address"
