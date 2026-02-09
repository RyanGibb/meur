{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Configuration system for Meur static site generator.
-- This module provides YAML-based configuration to replace all hardcoded values
-- from the original website generator.
module Meur.Config
  ( -- * Configuration Types
    SiteConfig (..),
    AuthorConfig (..),
    PathConfig (..),
    BibConfig (..),
    DateFormatConfig (..),
    FeatureConfig (..),

    -- * Feed Configuration
    FeedConfiguration (..),
    toFeedConfiguration,

    -- * Config Loading
    loadConfig,
    defaultConfig,

    -- * Pattern Builders
    buildPatterns,
    Patterns (..),

    -- * Routes
    staticRoute,
  )
where

import Data.Aeson
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither)
import GHC.Generics
import Hakyll hiding (FeedConfiguration, feedAuthorEmail, feedAuthorName, feedDescription, feedRoot, feedTitle)
import qualified Hakyll as H

-- | Main site configuration loaded from site.yaml
data SiteConfig = SiteConfig
  { siteTitle :: String,
    siteDescription :: String,
    siteAuthor :: AuthorConfig,
    siteRoot :: String,
    sitePaths :: PathConfig,
    siteDateFormats :: DateFormatConfig,
    siteFeatures :: FeatureConfig
  }
  deriving (Show, Generic)

-- | Author information
data AuthorConfig = AuthorConfig
  { authorName :: String,
    authorEmail :: String,
    authorUrl :: String
  }
  deriving (Show, Generic)

-- | Path configuration for directories and files
data PathConfig = PathConfig
  { staticDir :: FilePath,
    templatesDir :: FilePath,
    outputDir :: FilePath,
    cacheDir :: FilePath,
    scriptsDir :: FilePath,
    photosPattern :: FilePath,
    bibliographies :: [BibConfig],
    referencesFile :: FilePath,
    defaultCslStyle :: FilePath,
    luaFilters :: [FilePath],
    luaFiltersFeed :: [FilePath]
  }
  deriving (Show, Generic)

-- | Bibliography configuration
data BibConfig = BibConfig
  { bibName :: String, -- "papers" or "talks"
    bibFile :: FilePath, -- "papers.bib"
    bibCslStyle :: FilePath -- "ieee-with-url.csl"
  }
  deriving (Show, Generic)

-- | Date format configuration
data DateFormatConfig = DateFormatConfig
  { defaultFormat :: String, -- "%-d %b. %Y"
    isoFormat :: String -- "%Y-%m-%dT%H:%M:%SZ"
  }
  deriving (Show, Generic)

-- | Feature flags
data FeatureConfig = FeatureConfig
  { enablePhotos :: Bool,
    enableBibliography :: Bool,
    enableDualOutput :: Bool,
    enableFeeds :: Bool,
    enableTags :: Bool,
    enableGeocodingCache :: Maybe FilePath
  }
  deriving (Show, Generic)

-- | Hakyll Feed configuration (re-export to avoid confusion)
data FeedConfiguration = FeedConfiguration
  { feedTitle :: String,
    feedDescription :: String,
    feedAuthorName :: String,
    feedAuthorEmail :: String,
    feedAuthorUrl :: String,
    feedRoot :: String
  }
  deriving (Show, Eq)

-- | File patterns for different content types
data Patterns = Patterns
  { indexFiles :: Pattern,
    tagFiles :: Pattern,
    htmlFiles :: Pattern,
    postFiles :: Pattern,
    photoFiles :: Pattern,
    paperFiles :: Pattern,
    logFiles :: Pattern,
    articleFiles :: Pattern,
    copyFiles :: Pattern
  }

-- JSON instances
instance FromJSON SiteConfig where
  parseJSON = withObject "SiteConfig" $ \v ->
    SiteConfig
      <$> v .: "title"
      <*> v .: "description"
      <*> v .: "author"
      <*> v .: "root"
      <*> v .:? "paths" .!= defaultPathConfig
      <*> v .:? "dateFormats" .!= defaultDateFormats
      <*> v .:? "features" .!= defaultFeatures

instance FromJSON AuthorConfig where
  parseJSON = withObject "AuthorConfig" $ \v ->
    AuthorConfig
      <$> v .: "name"
      <*> v .: "email"
      <*> v .: "url"

instance FromJSON PathConfig where
  parseJSON = withObject "PathConfig" $ \v ->
    PathConfig
      <$> v .:? "staticDir" .!= "static"
      <*> v .:? "templatesDir" .!= "templates"
      <*> v .:? "outputDir" .!= "_site"
      <*> v .:? "cacheDir" .!= "_cache"
      <*> v .:? "scriptsDir" .!= "scripts"
      <*> v .:? "photosPattern" .!= "static/photos/*"
      <*> v .:? "bibliographies" .!= []
      <*> v .:? "referencesFile" .!= "references.bib"
      <*> v .:? "defaultCslStyle" .!= "ieee-with-url.csl"
      <*> v .:? "luaFilters" .!= ["org-keywords.lua", "elem-ids.lua", "footnote-commas.lua", "anchor-links.lua"]
      <*> v .:? "luaFiltersFeed" .!= ["org-keywords.lua", "elem-ids.lua", "footnote-commas.lua"]

instance FromJSON BibConfig where
  parseJSON = withObject "BibConfig" $ \v ->
    BibConfig
      <$> v .: "name"
      <*> v .: "file"
      <*> v .: "cslStyle"

instance FromJSON DateFormatConfig where
  parseJSON = withObject "DateFormatConfig" $ \v ->
    DateFormatConfig
      <$> v .:? "defaultFormat" .!= "%-d %b. %Y"
      <*> v .:? "isoFormat" .!= "%Y-%m-%dT%H:%M:%SZ"

instance FromJSON FeatureConfig where
  parseJSON = withObject "FeatureConfig" $ \v ->
    FeatureConfig
      <$> v .:? "enablePhotos" .!= True
      <*> v .:? "enableBibliography" .!= True
      <*> v .:? "enableDualOutput" .!= True
      <*> v .:? "enableFeeds" .!= True
      <*> v .:? "enableTags" .!= True
      <*> v .:? "enableGeocodingCache"

-- | Default path configuration
defaultPathConfig :: PathConfig
defaultPathConfig =
  PathConfig
    { staticDir = "static",
      templatesDir = "templates",
      outputDir = "_site",
      cacheDir = "_cache",
      scriptsDir = "scripts",
      photosPattern = "static/photos/*",
      bibliographies = [],
      referencesFile = "references.bib"
    }

-- | Default date formats
defaultDateFormats :: DateFormatConfig
defaultDateFormats =
  DateFormatConfig
    { defaultFormat = "%-d %b. %Y",
      isoFormat = "%Y-%m-%dT%H:%M:%SZ"
    }

-- | Default feature configuration
defaultFeatures :: FeatureConfig
defaultFeatures =
  FeatureConfig
    { enablePhotos = True,
      enableBibliography = True,
      enableDualOutput = True,
      enableFeeds = True,
      enableTags = True,
      enableGeocodingCache = Just "reverse-geocoding"
    }

-- | Default site configuration
defaultConfig :: SiteConfig
defaultConfig =
  SiteConfig
    { siteTitle = "My Site",
      siteDescription = "A Meur-powered static site",
      siteAuthor =
        AuthorConfig
          { authorName = "Author Name",
            authorEmail = "author@example.com",
            authorUrl = "https://example.com"
          },
      siteRoot = "https://example.com",
      sitePaths = defaultPathConfig,
      siteDateFormats = defaultDateFormats,
      siteFeatures = defaultFeatures
    }

-- | Convert SiteConfig to Hakyll's FeedConfiguration
toFeedConfiguration :: SiteConfig -> FeedConfiguration
toFeedConfiguration config =
  FeedConfiguration
    { feedTitle = siteTitle config,
      feedDescription = siteDescription config,
      feedAuthorName = authorName $ siteAuthor config,
      feedAuthorEmail = authorEmail $ siteAuthor config,
      feedAuthorUrl = authorUrl $ siteAuthor config,
      feedRoot = siteRoot config
    }

-- | Load configuration from YAML file
loadConfig :: FilePath -> IO (Either String SiteConfig)
loadConfig path = do
  result <- decodeFileEither path
  return $ case result of
    Left err -> Left $ show err
    Right config -> Right config

-- | Build Hakyll patterns from path configuration
buildPatterns :: PathConfig -> Patterns
buildPatterns paths =
  let static = staticDir paths
      indexPatterns =
        fromGlob (static ++ "/home.org")
          .||. fromGlob (static ++ "/posts.org")
          .||. fromGlob (static ++ "/talks.org")
          .||. fromGlob (static ++ "/tags.org")
          .||. fromGlob (static ++ "/logs.org")
          .||. fromGlob (static ++ "/index.org")
          .||. fromGlob (static ++ "/photos.org")
          .||. fromGlob (static ++ "/papers.org")
      tagPatterns =
        fromGlob (static ++ "/projects.org")
          .||. fromGlob (static ++ "/research.org")
          .||. fromGlob (static ++ "/technology.org")
          .||. fromGlob (static ++ "/self-hosting.org")
          .||. fromGlob (static ++ "/citable.org")
      htmlPatterns = fromGlob (static ++ "/**.md") .||. fromGlob (static ++ "/**.org")
      postPatterns = htmlPatterns .&&. complement indexPatterns .&&. complement tagPatterns
      photoPatterns = fromGlob $ photosPattern paths
      paperPatterns = "papers/*.html"
      logPatterns = fromRegex $ static ++ "/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].*"
      articlePatterns = postPatterns .&&. complement logPatterns
      copyPatterns = fromGlob (static ++ "/**") .&&. complement tagPatterns
   in Patterns
        { indexFiles = indexPatterns,
          tagFiles = tagPatterns,
          htmlFiles = htmlPatterns,
          postFiles = postPatterns,
          photoFiles = photoPatterns,
          paperFiles = paperPatterns,
          logFiles = logPatterns,
          articleFiles = articlePatterns,
          copyFiles = copyPatterns
        }

-- | Static route (removes "static/" prefix)
staticRoute :: Routes
staticRoute = gsubRoute "static/" (const "")
