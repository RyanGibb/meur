{-# LANGUAGE OverloadedStrings #-}

module Meur.Compiler.Post
  ( makeCombinedItems,
    getCombinedItemUTC,
    combinedItemRecentFirst,
    makeBibItems,
    indexCompiler,
    postCompiler,
    bibsCompiler,
    bibCompiler,
    combinedListCompiler,
    tagCompiler,
    tagsPageCompiler,
    photosCompiler,
  )
where

import Control.Monad (filterM, forM)
import qualified Data.List as L
import Data.Ord (Down (..), comparing)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Hakyll hiding (FeedConfiguration, feedAuthorEmail, feedAuthorName, feedDescription, feedRoot, feedTitle)
import Meur.Bib (Bib)
import qualified Meur.Bib
import Meur.BibHakyll (bibContext, bibDate)
import Meur.Compiler.Feed (absolutizeUrls)
import Meur.Compiler.Photo (photoContext)
import Meur.Compiler.Tag (bibHasTag, bibKindPlural, bibKindSingular, getBibTitle, tagContext)
import Meur.Config (FeedConfiguration, PathConfig, Patterns (..), defaultCslStyle, referencesFile)
import Meur.Context (bibPageContext, combinedItemContext, indexContext, markdownTitleContext, photosContext, postContext)
import Meur.Pandoc (bibRenderFeed, bibRenderHtml, bibRenderMarkdown)
import Meur.Types (BibKind (..), CombinedItem (..), Output (..))
import Meur.Util (isPublished)
import System.Directory (doesFileExist)
import System.FilePath (replaceExtension)

makeCombinedItems ::
  [Item String] ->
  [(BibKind, Bib)] ->
  [Item CopyFile] ->
  Compiler [Item CombinedItem]
makeCombinedItems posts bibs photos = do
  postItems <- mapM (makeItem . PostItem) posts
  bibItems <- mapM (\(kind, bib) -> makeItem (BibItem kind bib)) bibs
  photoItems <- mapM (makeItem . PhotoItem) photos
  return (postItems ++ bibItems ++ photoItems)

getCombinedItemUTC :: (MonadMetadata m, MonadFail m) => Item CombinedItem -> m UTCTime
getCombinedItemUTC combinedItem =
  case itemBody combinedItem of
    PostItem item -> getItemUTC defaultTimeLocale (itemIdentifier item)
    BibItem _ bib -> return $ bibDate bib
    PhotoItem item -> getItemUTC defaultTimeLocale (itemIdentifier item)

combinedItemRecentFirst :: (MonadMetadata m, MonadFail m) => [Item CombinedItem] -> m [Item CombinedItem]
combinedItemRecentFirst xs = do
  pairs <- forM xs $ \x -> do
    utc <- getCombinedItemUTC x
    return (x, utc)
  let sorted = map fst $ L.sortBy (comparing (Down . snd)) pairs
  return sorted

makeBibItems :: [Bib] -> Compiler [Item Bib]
makeBibItems bibs = do
  papers <- mapM makeItem bibs
  return $ L.sortBy (flip (comparing (bibDate . itemBody))) papers

indexCompiler :: PathConfig -> Tags -> [Item a] -> Context a -> Output -> Compiler (Item String)
indexCompiler pathConfig tags posts context output = do
  let refFile = referencesFile pathConfig
  let cslFile = defaultCslStyle pathConfig
  let baseCtx = indexContext posts context
  let (bibRenderFn, template, ctx) = case output of
        HTML -> (bibRenderHtml pathConfig, "templates/default.html", constField "html" "true" `mappend` baseCtx)
        MD -> (bibRenderMarkdown pathConfig, "templates/default.md", baseCtx)
  getResourceBody
    >>= applyAsTemplate ctx
    >>= bibRenderFn cslFile refFile
    >>= loadAndApplyTemplate template markdownTitleContext
    >>= relativizeUrls

postCompiler :: FeedConfiguration -> PathConfig -> Patterns -> Tags -> Identifier -> Output -> Compiler (Item String)
postCompiler feedConfig pathConfig patterns tags template output =
  let refFile = referencesFile pathConfig
      cslFile = defaultCslStyle pathConfig
      dateFormat = "%-d %b. %Y"
      ctx = postContext patterns dateFormat dateFormat tags
   in case output of
        HTML -> do
          getResourceBody
            >>= saveSnapshot "body"
            >>= bibRenderFeed pathConfig cslFile refFile
            >>= absolutizeUrls feedConfig
            >>= saveSnapshot "feed"
          getResourceBody
            >>= bibRenderHtml pathConfig cslFile refFile
            >>= saveSnapshot "teaser"
            >>= loadAndApplyTemplate template ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls
        MD ->
          getResourceBody
            >>= saveSnapshot "body"
            >>= bibRenderMarkdown pathConfig cslFile refFile
            >>= saveSnapshot "teaser"
            >>= loadAndApplyTemplate template ctx
            >>= loadAndApplyTemplate "templates/default.md" ctx
            >>= relativizeUrls

bibsCompiler :: PathConfig -> BibKind -> Tags -> [Bib] -> Output -> Compiler (Item String)
bibsCompiler pathConfig kind tags bibs output =
  let refFile = referencesFile pathConfig
      cslFile = defaultCslStyle pathConfig
      dateFormat = "%-d %b. %Y"
      baseCtx = listField (bibKindPlural kind) (bibPageContext (bibKindSingular kind) dateFormat tags) (makeBibItems bibs)
   in let (bibRenderFn, template, ctx) = case output of
            HTML -> (bibRenderHtml pathConfig, "templates/default.html", constField "html" "true" `mappend` baseCtx)
            MD -> (bibRenderMarkdown pathConfig, "templates/default.md", baseCtx)
       in getResourceBody
            >>= applyAsTemplate ctx
            >>= bibRenderFn cslFile refFile
            >>= loadAndApplyTemplate template markdownTitleContext
            >>= relativizeUrls

bibCompiler :: BibKind -> Bib -> Tags -> Output -> Compiler (Item String)
bibCompiler kind b tags output =
  let dateFormat = "%-d %b. %Y"
      baseCtx = bibPageContext (bibKindSingular kind) dateFormat tags
   in let (template, ctx) = case output of
            HTML -> ("templates/default.html", constField "html" "true" `mappend` baseCtx)
            MD -> ("templates/default.md", baseCtx)
       in makeItem b
            >>= loadAndApplyTemplate
              ( fromFilePath $
                  "templates/" ++ bibKindSingular kind ++ case output of
                    HTML -> ".html"
                    MD -> ".md"
              )
              ctx
            >>= loadAndApplyTemplate
              template
              ( constField "title" (getBibTitle b)
                  `mappend` constField "markdown" (bibKindPlural kind ++ "/" ++ Meur.Bib.name b ++ ".md")
                  `mappend` markdownTitleContext
              )
            >>= relativizeUrls

combinedListCompiler ::
  PathConfig ->
  Maybe FilePath ->
  Patterns ->
  Tags ->
  Compiler [Item CombinedItem] ->
  Output ->
  Compiler (Item String)
combinedListCompiler pathConfig geocodingCache patterns tags items output = do
  let refFile = referencesFile pathConfig
  let cslFile = defaultCslStyle pathConfig
  let dateFormat = "%-d %b. %Y"
  let baseCtx = listField "pages" (combinedItemContext geocodingCache patterns tags dateFormat dateFormat "%b %Y" dateFormat output) items `mappend` markdownTitleContext
  let (bibRenderFn, defaultTemplate, ctx) = case output of
        HTML -> (bibRenderHtml pathConfig, "templates/default.html", constField "html" "true" `mappend` baseCtx)
        MD -> (bibRenderMarkdown pathConfig, "templates/default.md", baseCtx)
  getResourceBody
    >>= applyAsTemplate ctx
    >>= bibRenderFn cslFile refFile
    >>= loadAndApplyTemplate defaultTemplate markdownTitleContext
    >>= relativizeUrls

tagCompiler :: PathConfig -> Maybe FilePath -> Patterns -> Tags -> String -> Pattern -> [(BibKind, Bib)] -> Output -> Compiler (Item String)
tagCompiler pathConfig geocodingCache patterns tags tag pattern bibs output = do
  let refFile = referencesFile pathConfig
  let cslFile = defaultCslStyle pathConfig
  let dateFormat = "%-d %b. %Y"
  posts <- recentFirst =<< filterM isPublished =<< loadAll pattern
  let filteredBibs = filter (\(_, bib) -> bibHasTag tag bib) bibs
  let combinedItems = combinedItemRecentFirst =<< makeCombinedItems posts filteredBibs []
  let filename = T.unpack $ T.toLower $ T.pack tag
  let file = "static/" ++ filename ++ ".org"
  let baseCtx =
        constField "title" tag
          `mappend` constField "filename" filename
          `mappend` constField "feed" filename
          `mappend` listField "pages" (combinedItemContext geocodingCache patterns tags dateFormat dateFormat "%b %Y" dateFormat output) combinedItems
          `mappend` markdownField "markdown"
          `mappend` markdownTitleContext
  let (bibRenderFn, ctx, defaultTemplate, tagTemplate) = case output of
        HTML -> (bibRenderHtml pathConfig, constField "html" "true" `mappend` baseCtx, "templates/default.html", "templates/tag.html")
        MD -> (bibRenderMarkdown pathConfig, baseCtx, "templates/default.md", "templates/tag.md")
  exists <- unsafeCompiler $ doesFileExist file
  if exists
    then do
      item <- load $ fromFilePath file
      item <-
        applyAsTemplate ctx item
          >>= bibRenderFn cslFile refFile
      makeItem (itemBody item)
        >>= loadAndApplyTemplate defaultTemplate ctx
        >>= relativizeUrls
    else do
      item <- loadAndApplyTemplate tagTemplate ctx (Item (fromFilePath file) "")
      makeItem (itemBody item)
        >>= loadAndApplyTemplate defaultTemplate ctx
        >>= relativizeUrls
  where
    markdownField :: String -> Context a
    markdownField = mapContext (`replaceExtension` ".md") . titleField

tagsPageCompiler :: PathConfig -> Tags -> Output -> Compiler (Item String)
tagsPageCompiler pathConfig tags output = do
  let refFile = referencesFile pathConfig
  let cslFile = defaultCslStyle pathConfig
  let baseCtx = listField "tags" (tagContext tags) (mapM makeItem $ tagsMap tags) `mappend` markdownTitleContext
  let (bibRenderFn, template, ctx) = case output of
        HTML -> (bibRenderHtml pathConfig, "templates/default.html", constField "html" "true" `mappend` baseCtx)
        MD -> (bibRenderMarkdown pathConfig, "templates/default.md", baseCtx)
  getResourceBody
    >>= applyAsTemplate ctx
    >>= bibRenderFn cslFile refFile
    >>= loadAndApplyTemplate template markdownTitleContext
    >>= relativizeUrls

photosCompiler :: PathConfig -> Maybe FilePath -> [Item a] -> Output -> Compiler (Item String)
photosCompiler pathConfig geocodingCache photos output = do
  let dateFormat = "%-d %b. %Y"
  let refFile = referencesFile pathConfig
  let cslFile = defaultCslStyle pathConfig
  let baseCtx = photosContext geocodingCache dateFormat photos
  let (bibRenderFn, template, ctx) = case output of
        HTML -> (bibRenderHtml pathConfig, "templates/default.html", constField "html" "true" `mappend` baseCtx)
        MD -> (bibRenderMarkdown pathConfig, "templates/default.md", baseCtx)
  getResourceBody
    >>= applyAsTemplate ctx
    >>= bibRenderFn cslFile refFile
    >>= loadAndApplyTemplate template markdownTitleContext
    >>= relativizeUrls
