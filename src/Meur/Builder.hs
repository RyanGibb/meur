{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Meur.Builder (buildSite) where

import Control.Monad (filterM, forM_, when)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Hakyll hiding (FeedConfiguration, feedAuthorEmail, feedAuthorName, feedDescription, feedRoot, feedTitle)
import Hakyll.Core.Configuration (Configuration (..), defaultConfiguration)
import Meur.Bib (name)
import Meur.BibHakyll (Bibs (..), bibContext, bibFileCompiler, parseBibFile)
import Meur.Compiler.Feed (combinedFeedCompiler)
import Meur.Compiler.Post
import Meur.Compiler.Tag (bibHasTag, bibKindPlural, bibKindSingular, buildBibTags, mergeTags)
import Meur.Config
import Meur.Context (markdownTitleContext, postContext)
import Meur.Types (BibKind (..), FeedType (..), Output (..))
import Meur.Util (isNotDraft, isNotDraftMeta, isPublished)
import System.FilePath ((</>))
import System.Process (callCommand)

buildSite :: SiteConfig -> IO ()
buildSite config =
  let hakyllConfig =
        defaultConfiguration
          { destinationDirectory = outputDir (sitePaths config),
            storeDirectory = cacheDir (sitePaths config),
            tmpDirectory = cacheDir (sitePaths config) </> "tmp"
          }
   in hakyllWith hakyllConfig $ do
        let paths = sitePaths config
        let features = siteFeatures config
        let patterns = buildPatterns paths
        let dateFormat = defaultFormat $ siteDateFormats config
        let isoDateFormat = isoFormat $ siteDateFormats config
        let feedConfig = toFeedConfiguration config
        let geocodingCache = enableGeocodingCache features

        -- Load bibliographies
        bibsData <- preprocess $ do
          let bibConfigs = bibliographies paths
          mapM
            ( \bibConfig -> do
                content <- readFile (bibFile bibConfig)
                let Bibs bibs = parseBibFile content
                return (bibName bibConfig, bibs)
            )
            bibConfigs

        let paperBibs = maybe [] id $ lookup "papers" bibsData
        let talkBibs = maybe [] id $ lookup "talks" bibsData
        let bibs = map (Paper,) paperBibs ++ map (Talk,) talkBibs

        -- Build tags
        postTags <- buildTags (postFiles patterns) (fromCapture "*.html" . T.unpack . T.toLower . T.pack)
        let bibTags = buildBibTags bibs (fromCapture "*.html" . T.unpack . T.toLower . T.pack)
        let tags = mergeTags postTags bibTags

        -- Tag file compilation
        match (tagFiles patterns) $ do
          compile getResourceBody

        -- Tag pages
        tagsRules tags $ \tag pattern -> do
          route staticRoute
          compile $ tagCompiler paths geocodingCache patterns tags tag pattern bibs HTML

          when (enableDualOutput features) $ do
            create [fromCapture "*.md" . T.unpack . T.toLower . T.pack $ tag] $ do
              route staticRoute
              compile $ tagCompiler paths geocodingCache patterns tags tag pattern bibs MD

          when (enableFeeds features) $ do
            let createTagFeed pat feedtype = do
                  create [fromCapture pat . T.unpack . T.toLower . T.pack $ tag] $ do
                    route idRoute
                    compile $ do
                      posts <- recentFirst =<< filterM isPublished =<< loadAllSnapshots (pattern .&&. postFiles patterns .&&. hasNoVersion) "body"
                      let taggedBibs = filter (\(_, bib) -> bibHasTag tag bib) bibs
                      let items = combinedItemRecentFirst =<< makeCombinedItems posts taggedBibs []
                      combinedFeedCompiler feedtype feedConfig geocodingCache patterns dateFormat isoDateFormat tags (referencesFile paths) items

            createTagFeed "*.xml" XmlFeed
            createTagFeed "*.json" JsonFeed

        -- Home page
        match "static/home.org" $ do
          route $ staticRoute `composeRoutes` setExtension "html"
          compile $ do
            posts <- filterM isPublished =<< loadAll (articleFiles patterns .&&. hasNoVersion)
            let items = combinedItemRecentFirst =<< makeCombinedItems posts bibs []
            combinedListCompiler paths geocodingCache patterns tags items HTML

        when (enableDualOutput features) $ do
          match "static/home.org" $ version "markdown" $ do
            route $ staticRoute `composeRoutes` setExtension "md"
            compile $ do
              posts <- filterM isPublished =<< loadAll (articleFiles patterns .&&. hasVersion "markdown")
              let items = combinedItemRecentFirst =<< makeCombinedItems posts bibs []
              combinedListCompiler paths geocodingCache patterns tags items MD

        -- Posts index
        match "static/posts.org" $ do
          route $ staticRoute `composeRoutes` setExtension "html"
          compile $ do
            posts <- recentFirst =<< filterM isPublished =<< loadAllSnapshots (articleFiles patterns .&&. hasNoVersion) "teaser"
            indexCompiler paths tags posts (postContext patterns dateFormat dateFormat tags) HTML

        when (enableDualOutput features) $ do
          match "static/posts.org" $ version "markdown" $ do
            route $ staticRoute `composeRoutes` setExtension "md"
            compile $ do
              posts <- recentFirst =<< filterM isPublished =<< (loadAllSnapshots (articleFiles patterns .&&. hasVersion "markdown") "teaser" :: Compiler [Item String])
              indexCompiler paths tags posts (postContext patterns dateFormat dateFormat tags) MD

        -- Logs index
        match "static/logs.org" $ do
          route $ staticRoute `composeRoutes` setExtension "html"
          compile $ do
            posts <- reverse <$> loadAllSnapshots (logFiles patterns .&&. hasNoVersion) "body"
            indexCompiler paths tags posts (postContext patterns dateFormat dateFormat tags) HTML

        when (enableDualOutput features) $ do
          match "static/logs.org" $ version "markdown" $ do
            route $ staticRoute `composeRoutes` setExtension "md"
            compile $ do
              posts <- reverse <$> (loadAllSnapshots (logFiles patterns .&&. hasVersion "markdown") "body" :: Compiler [Item String])
              indexCompiler paths tags posts (postContext patterns dateFormat dateFormat tags) MD

        -- Tags page
        match "static/tags.org" $ do
          route $ staticRoute `composeRoutes` setExtension "html"
          compile $ tagsPageCompiler paths tags HTML

        when (enableDualOutput features) $ do
          match "static/tags.org" $ version "markdown" $ do
            route $ staticRoute `composeRoutes` setExtension "md"
            compile $ tagsPageCompiler paths tags MD

        -- Index page
        match "static/index.org" $ do
          route $ staticRoute `composeRoutes` setExtension "html"
          compile $ do
            posts <- filterM isNotDraft =<< loadAll (htmlFiles patterns .&&. hasNoVersion .&&. complement "static/index.org")
            let items = makeCombinedItems posts bibs []
            combinedListCompiler paths geocodingCache patterns tags items HTML

        when (enableDualOutput features) $ do
          match "static/index.org" $ version "markdown" $ do
            route $ staticRoute `composeRoutes` setExtension "md"
            compile $ do
              posts <- filterM isNotDraft =<< loadAll (htmlFiles patterns .&&. hasVersion "markdown" .&&. complement "static/index.org")
              let items = makeCombinedItems posts bibs []
              combinedListCompiler paths geocodingCache patterns tags items MD

        -- Photos
        when (enablePhotos features) $ do
          match "static/photos.org" $ do
            route $ staticRoute `composeRoutes` setExtension "html"
            compile $ do
              photos <- recentFirst =<< (loadAll (photoFiles patterns .&&. hasNoVersion) :: Compiler [Item CopyFile])
              photosCompiler paths geocodingCache photos HTML

          when (enableDualOutput features) $ do
            match "static/photos.org" $ version "markdown" $ do
              route $ staticRoute `composeRoutes` setExtension "md"
              compile $ do
                photos <- recentFirst =<< (loadAll (photoFiles patterns .&&. hasVersion "markdown") :: Compiler [Item CopyFile])
                photosCompiler paths geocodingCache photos MD

        -- Bibliography sections
        when (enableBibliography features) $ do
          let bibSectionRules kind bibs' = do
                match (fromGlob $ bibKindPlural kind ++ ".bib") $ do
                  route idRoute
                  compile bibFileCompiler

                match (fromGlob $ "static/" ++ bibKindPlural kind ++ ".org") $ do
                  route $ staticRoute `composeRoutes` setExtension "html"
                  compile $ bibsCompiler paths kind tags bibs' HTML

                when (enableDualOutput features) $ do
                  match (fromGlob $ "static/" ++ bibKindPlural kind ++ ".org") $ version "markdown" $ do
                    route $ staticRoute `composeRoutes` setExtension "md"
                    compile $ bibsCompiler paths kind tags bibs' MD

                forM_ bibs' $ \b -> do
                  create [fromCapture (fromGlob $ bibKindPlural kind ++ "/*.bib") $ name b] $ do
                    route idRoute
                    compile $ makeItem b >>= applyTemplate "$bib$" (bibContext (bibKindSingular kind) dateFormat)

                  create [fromCapture (fromGlob $ bibKindPlural kind ++ "/*.html") $ name b] $ do
                    route idRoute
                    compile $ bibCompiler kind b tags HTML

                  when (enableDualOutput features) $ do
                    create [fromCapture (fromGlob $ bibKindPlural kind ++ "/*.md") $ name b] $ do
                      route idRoute
                      compile $ bibCompiler kind b tags MD

          bibSectionRules Paper paperBibs
          bibSectionRules Talk talkBibs

        -- Articles
        matchMetadata (articleFiles patterns) isNotDraftMeta $ do
          route $ staticRoute `composeRoutes` setExtension "html"
          compile $ postCompiler feedConfig paths patterns tags "templates/post.html" HTML

        when (enableDualOutput features) $ do
          matchMetadata (articleFiles patterns) isNotDraftMeta $ version "markdown" $ do
            route $ staticRoute `composeRoutes` setExtension "md"
            compile $ postCompiler feedConfig paths patterns tags "templates/post.md" MD

        -- Logs
        matchMetadata (logFiles patterns) isNotDraftMeta $ do
          route $ staticRoute `composeRoutes` setExtension "html"
          compile $ postCompiler feedConfig paths patterns tags "templates/log.html" HTML

        when (enableDualOutput features) $ do
          matchMetadata (logFiles patterns) isNotDraftMeta $ version "markdown" $ do
            route $ staticRoute `composeRoutes` setExtension "md"
            compile $ postCompiler feedConfig paths patterns tags "templates/log.md" MD

        -- Feeds
        when (enableFeeds features) $ do
          let createFeed name' items = do
                create [fromFilePath (name' ++ ".xml")] $ do
                  route idRoute
                  compile $ combinedFeedCompiler XmlFeed feedConfig geocodingCache patterns dateFormat isoDateFormat tags (referencesFile paths) items
                create [fromFilePath (name' ++ ".json")] $ do
                  route idRoute
                  compile $ combinedFeedCompiler JsonFeed feedConfig geocodingCache patterns dateFormat isoDateFormat tags (referencesFile paths) items

          createFeed "home" $ do
            posts <- filterM isPublished =<< loadAllSnapshots (articleFiles patterns .&&. hasNoVersion) "feed"
            combinedItemRecentFirst =<< makeCombinedItems posts bibs []
          createFeed "posts" $ do
            posts <- recentFirst =<< filterM isPublished =<< loadAllSnapshots (articleFiles patterns .&&. hasNoVersion) "feed"
            makeCombinedItems posts [] []
          createFeed "logs" $ do
            posts <- recentFirst =<< filterM isPublished =<< loadAllSnapshots (logFiles patterns .&&. hasNoVersion) "feed"
            makeCombinedItems posts [] []
          when (enableBibliography features) $ do
            createFeed "papers" $ makeCombinedItems [] (map (Paper,) paperBibs) []
            createFeed "talks" $ makeCombinedItems [] (map (Talk,) talkBibs) []
          when (enablePhotos features) $ do
            createFeed "photos" $ combinedItemRecentFirst =<< makeCombinedItems [] [] =<< (loadAll (photoFiles patterns .&&. hasNoVersion) :: Compiler [Item CopyFile])

        -- Sitemap
        create ["sitemap.xml"] $ do
          route idRoute
          compile $ do
            posts <- loadAll (htmlFiles patterns .&&. hasNoVersion)
            let sitemapCtx =
                  listField "pages" (urlField "loc" `mappend` postContext patterns dateFormat dateFormat tags) (return posts)
                    `mappend` constField "root" (siteRoot config)
                    `mappend` defaultContext
            makeItem ""
              >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

        -- 404 page
        match "404.md" $ do
          route $ setExtension "html"
          compile $ do
            getResourceBody
              >>= loadAndApplyTemplate "templates/default.html" markdownTitleContext

        -- Copy files
        matchMetadata (copyFiles patterns) isNotDraftMeta $ do
          route staticRoute
          compile copyFileCompiler

        -- CSS
        match "static/*.css" $ do
          route staticRoute
          compile compressCssCompiler

        -- CSL and bibliography
        match "*.csl" $
          compile cslCompiler

        match (fromGlob $ referencesFile paths) $
          compile biblioCompiler

        -- Templates
        match "templates/*" $
          compile templateBodyCompiler
