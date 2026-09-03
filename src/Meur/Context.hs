{-# LANGUAGE OverloadedStrings #-}

module Meur.Context
  ( postContext,
    adjacentLogContext,
    bibPageContext,
    markdownTitleContext,
    markdownField,
    myDateField,
    dateFieldFromTitle,
    indexContext,
    photosContext,
    combinedItemContext,
    combinedItemContextfield,
    adjacentLogFieldHtml,
    adjacentLogFieldMarkdown,
    getAdjacentLog,
  )
where

import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import Data.Time.Format (formatTime, parseTimeM)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Hakyll
import Meur.Bib (Bib)
import qualified Meur.Bib
import Meur.BibHakyll (bibContext)
import Meur.Compiler.Photo (photoContext)
import Meur.Compiler.Tag (bibKindPlural, bibKindSingular, bibTagsField, pageTagsField)
import Meur.Types (BibKind (..), CombinedItem (..), Output (..))
import Meur.Util (dateFromTitle, isMonthTitle, itemUTC)
import System.FilePath (replaceExtension, takeBaseName)

postContext :: String -> String -> Tags -> Context String
postContext titleDateFormat postDateFormat _tags =
  dateFieldFromTitleWithMetadata "title" titleDateFormat
    `mappend` publishedField "published" postDateFormat
    `mappend` myDateField "updated" postDateFormat
    `mappend` pageTagsField "tags"
    `mappend` teaserField "teaser" "teaser"
    `mappend` markdownTitleContext

bibPageContext :: String -> String -> Tags -> Context Bib
bibPageContext csl dateFormat' _tags =
  bibTagsField "tags"
    `mappend` bibContext csl dateFormat'

markdownTitleContext :: Context String
markdownTitleContext =
  markdownField "markdown"
    `mappend` defaultContext

markdownField :: String -> Context a
markdownField = mapContext (`replaceExtension` ".md") . titleField

myDateField :: String -> String -> Context String
myDateField name format =
  field name $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    let date :: Maybe UTCTime
        date = lookupString name metadata >>= parseTimeM True defaultTimeLocale "%Y-%m-%d"
    case date of
      Nothing -> noResult ""
      Just d -> return $ formatTime defaultTimeLocale format d

logDateFormat :: Item a -> String -> String
logDateFormat item dayFormat
  | isMonthTitle item = "%B %Y"
  | otherwise = dayFormat

-- | Like Hakyll's 'dateField' but resolves the timestamp via 'itemUTC', so
-- monthly logs (@YYYY-MM@) — which Hakyll's 'getItemUTC' can't parse — work.
publishedField :: String -> String -> Context a
publishedField name format =
  field name $ \item -> formatTime defaultTimeLocale format <$> itemUTC item

dateFieldFromTitle :: String -> String -> Context String
dateFieldFromTitle key format =
  field key $ \item ->
    case dateFromTitle item of
      Nothing -> noResult ""
      Just date ->
        return $ formatTime defaultTimeLocale (logDateFormat item format) date

dateFieldFromTitleWithMetadata :: String -> String -> Context String
dateFieldFromTitleWithMetadata key format =
  field key $ \item ->
    case dateFromTitle item of
      Nothing -> noResult ""
      Just date -> do
        metadata <- getMetadata (itemIdentifier item)
        let formattedDate = formatTime defaultTimeLocale (logDateFormat item format) date
        return $ case lookupString "title" metadata of
          Just t  -> formattedDate ++ " " ++ t
          Nothing -> formattedDate

indexContext :: [Item a] -> Context a -> Context String
indexContext pages itemContext =
  listField "pages" itemContext (return pages)
    `mappend` markdownTitleContext

photosContext :: Maybe FilePath -> String -> [Item a] -> Context String
photosContext geocodingCache dateFormat photos =
  listField "photos" (photoContext geocodingCache dateFormat) (return photos)
    `mappend` markdownTitleContext

combinedItemContextfield :: Maybe FilePath -> Item CombinedItem -> String -> Tags -> String -> String -> String -> String -> Compiler ContextField
combinedItemContextfield geocodingCache i key tags titleDateFormat postDateFormat bibDateFormat photoDateFormat =
  case itemBody i of
    PostItem i' -> unContext (postContext titleDateFormat postDateFormat tags) key [] i'
    BibItem kind b -> do
      i' <- makeItem b
      unContext (bibPageContext (bibKindSingular kind) bibDateFormat tags) key [] i'
    PhotoItem i' -> unContext (photoContext geocodingCache photoDateFormat) key [] i'

combinedItemContext :: Maybe FilePath -> Tags -> String -> String -> String -> String -> Output -> Context CombinedItem
combinedItemContext geocodingCache tags titleDateFormat postDateFormat bibDateFormat photoDateFormat output =
  field
    "class"
    ( \item -> case itemBody item of
        PostItem _ -> return "post-item"
        BibItem Paper _ -> return "paper-item"
        BibItem Talk _ -> return "talk-item"
        PhotoItem _ -> return "photo-item"
    )
    `mappend` field
      "url"
      ( \item -> case itemBody item of
          BibItem k b -> return $ "/" ++ bibKindPlural k ++ "/" ++ Meur.Bib.name b ++ ext
          PostItem _ -> noResult ""
          PhotoItem _ -> noResult ""
      )
    `mappend` (Context $ \key _ i -> combinedItemContextfield geocodingCache i key tags titleDateFormat postDateFormat bibDateFormat photoDateFormat)
  where
    ext = case output of
      HTML -> ".html"
      MD   -> ".md"

-- | Prev/next log navigation from a Rules-time sorted list, so log pages
-- don't depend on each other; 'Meur.Builder' invalidates them on set changes.
adjacentLogContext :: [Identifier] -> String -> Context String
adjacentLogContext sortedLogs postDateFormat =
  field "htmlPrev" (adjacentLogFieldHtml sortedLogs (-1) postDateFormat)
    `mappend` field "htmlNext" (adjacentLogFieldHtml sortedLogs 1 postDateFormat)
    `mappend` field "mdPrev" (adjacentLogFieldMarkdown sortedLogs (-1) postDateFormat)
    `mappend` field "mdNext" (adjacentLogFieldMarkdown sortedLogs 1 postDateFormat)

adjacentLogFieldHtml :: [Identifier] -> Int -> String -> Item String -> Compiler String
adjacentLogFieldHtml sortedLogs offset format item =
  case getAdjacentLog sortedLogs item offset of
    Nothing -> noResult ""
    Just adj -> do
      mroute <- getRoute adj
      return $ maybe "" (\r -> "<a href=\"" ++ toUrl r ++ "\">" ++ adjacentLogLabel adj format ++ "</a>") mroute

adjacentLogFieldMarkdown :: [Identifier] -> Int -> String -> Item String -> Compiler String
adjacentLogFieldMarkdown sortedLogs offset format item =
  case getAdjacentLog sortedLogs item offset of
    Nothing -> noResult ""
    Just adj -> do
      mroute <- getRoute (setVersion (Just "markdown") adj)
      return $ maybe "" (\r -> "[" ++ adjacentLogLabel adj format ++ "](" ++ toUrl r ++ ")") mroute

adjacentLogLabel :: Identifier -> String -> String
adjacentLogLabel adj format =
  let a = Item adj ()
      date = fmap (formatTime defaultTimeLocale (logDateFormat a format)) (dateFromTitle a)
   in fromMaybe (takeBaseName (toFilePath adj)) date

getAdjacentLog :: [Identifier] -> Item a -> Int -> Maybe Identifier
getAdjacentLog sortedLogs current offset = do
  idx <- L.elemIndex (setVersion Nothing (itemIdentifier current)) sortedLogs
  let newIndex = idx + offset
  if newIndex >= 0 && newIndex < length sortedLogs
    then Just (sortedLogs !! newIndex)
    else Nothing
