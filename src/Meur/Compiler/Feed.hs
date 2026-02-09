{-# LANGUAGE OverloadedStrings #-}

module Meur.Compiler.Feed
  ( feedContext,
    referencesField,
    referenceContext,
    combinedFeedCompiler,
    absolutizeUrls,
    getCombinedItemUTC,
  )
where

import qualified Data.List as L
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.Time
import Data.Time.Format (formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Hakyll hiding (FeedConfiguration, feedAuthorEmail, feedAuthorName, feedDescription, feedRoot, feedTitle)
import Meur.Bib (Bib, bibsIndex, loadBibs)
import qualified Meur.Bib
import Meur.BibHakyll (bibContext, bibDate)
import Meur.Compiler.Tag (bibKindSingular)
import Meur.Config (FeedConfiguration (..), Patterns (..))
import Meur.Context (bibPageContext, combinedItemContext, combinedItemContextfield)
import Meur.Pandoc (extractCitationKeys, readerOptions)
import Meur.Types (BibKind (..), CombinedItem (..), FeedType (..))
import Meur.Util (escapeString)
import System.FilePath (normalise, takeDirectory, (</>))

getCombinedItemUTC :: (MonadMetadata m, MonadFail m) => Item CombinedItem -> m UTCTime
getCombinedItemUTC combinedItem =
  case itemBody combinedItem of
    PostItem item -> getItemUTC defaultTimeLocale (itemIdentifier item)
    BibItem _ bib -> return $ bibDate bib
    PhotoItem item -> getItemUTC defaultTimeLocale (itemIdentifier item)

feedContext :: FeedConfiguration -> Context a
feedContext config =
  constField "title" (feedTitle config)
    `mappend` constField "description" (feedDescription config)
    `mappend` constField "authorName" (feedAuthorName config)
    `mappend` constField "authorEmail" (feedAuthorEmail config)
    `mappend` constField "authorUrl" (feedAuthorUrl config)
    `mappend` constField "root" (feedRoot config)

referencesField :: Item CombinedItem -> Compiler [Item T.Text]
referencesField item = do
  case itemBody item of
    PostItem i -> do
      body <- loadSnapshot (itemIdentifier i) "body" :: Compiler (Item String)
      doc <- readPandocWith readerOptions body
      case extractCitationKeys (itemBody doc) of
        [] -> noResult ""
        keys -> mapM makeItem keys
    BibItem _ _ -> noResult ""
    PhotoItem _ -> noResult ""

referenceContext :: String -> [Bib] -> Context T.Text
referenceContext dateFormat bibs =
  Context $ \k _ i -> do
    let citekey = itemBody i
    case bibsIndex bibs (T.unpack citekey) of
      Nothing -> noResult ""
      Just bibEntry -> do
        bib <- makeItem bibEntry
        unContext (mapContextBy (== "parsed") escapeString $ bibContext "paper" dateFormat) k [] bib

combinedFeedCompiler ::
  FeedType ->
  FeedConfiguration ->
  Maybe FilePath ->
  Patterns ->
  String ->
  String ->
  Tags ->
  FilePath ->
  Compiler [Item CombinedItem] ->
  Compiler (Item String)
combinedFeedCompiler feedType config geocodingCache patterns dateFormat isoDateFormat tags referencesFile combinedItems = do
  let template = case feedType of
        XmlFeed -> "templates/atom.xml"
        JsonFeed -> "templates/feed.json"
  let applyEscape = case feedType of
        XmlFeed -> id
        JsonFeed -> escapeString
  bibs <- do
    bibResult <- unsafeCompiler $ loadBibs referencesFile
    case bibResult of
      Left err -> fail $ "ERROR: " ++ err
      Right bs -> return bs
  let feedCombinedItemContext =
        contentField
          `mappend` listFieldWith "references" (referenceContext dateFormat bibs) referencesField
          `mappend` combinedItemContext geocodingCache patterns tags dateFormat isoDateFormat isoDateFormat isoDateFormat
          `mappend` updatedField
          `mappend` feedContext config
        where
          contentField = field "content" $ \i -> do
            c <- case itemBody i of
              PostItem i' -> return $ StringField $ itemBody i'
              BibItem kind b -> do
                bibStr <-
                  makeItem b
                    >>= loadAndApplyTemplate
                      (fromFilePath $ "templates/" ++ bibKindSingular kind ++ ".html")
                      (constField "html" "true" `mappend` bibPageContext (bibKindSingular kind) dateFormat tags)
                return $ StringField $ itemBody bibStr
              PhotoItem i' -> do
                b <- loadAndApplyTemplate "templates/photo.html" (urlField "url") i' >>= absolutizeUrls config
                return $ StringField $ itemBody b
            case c of
              EmptyField -> fail "Hakyll.Web.Template.Context.mapContext: can't map over a boolField!"
              StringField str -> return $ applyEscape str
          updatedField = field "updated" $ \i -> do
            c <- combinedItemContextfield geocodingCache patterns i "published" tags dateFormat isoDateFormat isoDateFormat isoDateFormat
            case c of
              EmptyField -> fail "Hakyll.Web.Template.Context.mapContext: can't map over a boolField!"
              StringField str -> return str
  items <- combinedItems
  let feedCtx =
        feedContext config
          `mappend` urlField "url"
          `mappend` ( field "updated" $ \_ -> do
                        case items of
                          (i : _) -> formatTime defaultTimeLocale isoDateFormat <$> getCombinedItemUTC i
                          [] -> unsafeCompiler $ do
                            -- Use current time for empty feeds
                            currentTime <- Data.Time.getCurrentTime
                            return $ formatTime defaultTimeLocale isoDateFormat currentTime
                    )
          `mappend` listField "items" feedCombinedItemContext (return items)
          `mappend` missingField
  loadAndApplyTemplate template feedCtx =<< makeItem ""

absolutizeUrls :: FeedConfiguration -> Item String -> Compiler (Item String)
absolutizeUrls config item = do
  route <- getRoute $ itemIdentifier item
  return $ case route of
    Nothing -> item
    Just r -> fmap (withUrls (makeAbsolute r)) item
  where
    root = feedRoot config

    makeAbsolute :: FilePath -> String -> String
    makeAbsolute route url
      | "://" `T.isInfixOf` T.pack url = url
      | "mailto:" `T.isPrefixOf` T.pack url = url
      | "/" `L.isPrefixOf` url = root ++ url
      | otherwise =
          let itemDir = takeDirectory route
              resolvedPath = normalise $ itemDir </> url
              cleanPath =
                if "./" `L.isPrefixOf` resolvedPath
                  then drop 2 resolvedPath
                  else resolvedPath
           in root ++ "/" ++ cleanPath
