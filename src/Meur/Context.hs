{-# LANGUAGE OverloadedStrings #-}

module Meur.Context
  ( postContext,
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
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format (formatTime, parseTimeM)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Hakyll
import Meur.Bib (Bib)
import qualified Meur.Bib
import Meur.BibHakyll (bibContext)
import Meur.Compiler.Photo (photoContext)
import Meur.Compiler.Tag (bibKindPlural, bibKindSingular, bibTagsField, pageTagsField)
import Meur.Config (Patterns (..))
import Meur.Types (BibKind (..), CombinedItem (..))
import Meur.Util (dateFromTitle)
import System.FilePath (replaceExtension, takeBaseName)

postContext :: Patterns -> String -> String -> Tags -> Context String
postContext patterns titleDateFormat postDateFormat tags =
  field "htmlPrev" (adjacentLogFieldHtml (logFiles patterns) (-1) postDateFormat)
    `mappend` field "htmlNext" (adjacentLogFieldHtml (logFiles patterns) 1 postDateFormat)
    `mappend` field "mdPrev" (adjacentLogFieldMarkdown (logFiles patterns) (-1) postDateFormat)
    `mappend` field "mdNext" (adjacentLogFieldMarkdown (logFiles patterns) 1 postDateFormat)
    `mappend` dateFieldFromTitle "title" titleDateFormat
    `mappend` dateField "published" postDateFormat
    `mappend` myDateField "updated" postDateFormat
    `mappend` pageTagsField "tags"
    `mappend` teaserField "teaser" "teaser"
    `mappend` markdownTitleContext

bibPageContext :: String -> String -> Tags -> Context Bib
bibPageContext csl dateFormat' tags =
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

dateFieldFromTitle :: String -> String -> Context String
dateFieldFromTitle key format =
  field key $ \item ->
    case dateFromTitle item of
      Nothing -> noResult ""
      Just date ->
        return $ formatTime defaultTimeLocale format date

indexContext :: [Item a] -> Context a -> Context String
indexContext pages itemContext =
  listField "pages" itemContext (return pages)
    `mappend` markdownTitleContext

photosContext :: Maybe FilePath -> String -> [Item a] -> Context String
photosContext geocodingCache dateFormat photos =
  listField "photos" (photoContext geocodingCache dateFormat) (return photos)
    `mappend` markdownTitleContext

combinedItemContextfield :: Maybe FilePath -> Patterns -> Item CombinedItem -> String -> Tags -> String -> String -> String -> String -> Compiler ContextField
combinedItemContextfield geocodingCache patterns i key tags titleDateFormat postDateFormat bibDateFormat photoDateFormat =
  case itemBody i of
    PostItem i' -> unContext (postContext patterns titleDateFormat postDateFormat tags) key [] i'
    BibItem kind b -> do
      i' <- makeItem b
      unContext (bibPageContext (bibKindSingular kind) bibDateFormat tags) key [] i'
    PhotoItem i' -> unContext (photoContext geocodingCache photoDateFormat) key [] i'

combinedItemContext :: Maybe FilePath -> Patterns -> Tags -> String -> String -> String -> String -> Context CombinedItem
combinedItemContext geocodingCache patterns tags titleDateFormat postDateFormat bibDateFormat photoDateFormat =
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
          BibItem k b -> return $ "/" ++ bibKindPlural k ++ "/" ++ Meur.Bib.name b ++ ".html"
          PostItem _ -> noResult ""
          PhotoItem _ -> noResult ""
      )
    `mappend` (Context $ \key _ i -> combinedItemContextfield geocodingCache patterns i key tags titleDateFormat postDateFormat bibDateFormat photoDateFormat)

adjacentLogFieldHtml :: Pattern -> Int -> String -> Item String -> Compiler String
adjacentLogFieldHtml logPattern offset format item = do
  posts <- loadAllSnapshots (logPattern .&&. hasNoVersion) "body" :: Compiler [Item String]
  let adjacent = getAdjacentLog posts item offset
  case adjacent of
    Nothing -> noResult ""
    Just a -> do
      mroute <- getRoute (itemIdentifier a)
      let filePath = toFilePath (itemIdentifier item)
          title = takeBaseName filePath
          date = fmap (formatTime defaultTimeLocale format) (dateFromTitle a)
          label = fromMaybe title date
      return $ maybe "" (\r -> "<a href=\"" ++ r ++ "\">" ++ label ++ "</a>") mroute

adjacentLogFieldMarkdown :: Pattern -> Int -> String -> Item String -> Compiler String
adjacentLogFieldMarkdown logPattern offset format item = do
  posts <- loadAllSnapshots (logPattern .&&. hasVersion "markdown") "body" :: Compiler [Item String]
  let adjacent = getAdjacentLog posts item offset
  case adjacent of
    Nothing -> noResult ""
    Just a -> do
      mroute <- getRoute (itemIdentifier a)
      let filePath = toFilePath (itemIdentifier item)
          title = takeBaseName filePath
          date = fmap (formatTime defaultTimeLocale format) (dateFromTitle a)
          label = fromMaybe title date
      return $ maybe "" (\r -> "[" ++ label ++ "](" ++ toUrl r ++ ")") mroute

getAdjacentLog :: [Item a] -> Item b -> Int -> Maybe (Item a)
getAdjacentLog posts current offset =
  case L.elemIndex (itemIdentifier current) (map itemIdentifier posts) of
    Nothing -> Nothing
    Just idx ->
      let newIndex = idx + offset
       in if newIndex >= 0 && newIndex < length posts
            then Just (posts !! newIndex)
            else Nothing
