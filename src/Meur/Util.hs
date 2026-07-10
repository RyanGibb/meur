{-# LANGUAGE OverloadedStrings #-}

module Meur.Util
  ( -- JSON escaping
    escapeString,
    -- Date utilities
    dateFromTitle,
    isMonthTitle,
    itemUTC,
    recentFirstT,
    -- URL utilities
    replaceExt,
    generateId,
    -- Draft/published checking
    isPublished,
    isNotDraft,
    isNotDraftMeta,
  )
where

import Control.Applicative ((<|>))
import Data.Char (isAlphaNum)
import Data.List (sortBy)
import Data.Maybe (isJust, isNothing)
import Data.Ord (Down (..), comparing)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format (parseTimeM)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Hakyll
import System.FilePath (takeBaseName)
import Text.Printf (printf)

-- | Escape string for JSON according to RFC8259 §7
escapeString :: String -> String
escapeString = flip escapeString' ""
  where
    escapeString' :: String -> ShowS
    escapeString' [] s = s
    escapeString' ('"' : cs) s = showString "\\\"" (escapeString' cs s)
    escapeString' ('\\' : cs) s = showString "\\\\" (escapeString' cs s)
    escapeString' (c : cs) s
      | c < ' ' = escapeChar c (escapeString' cs s)
      | otherwise = showChar c (escapeString' cs s)

    escapeChar :: Char -> ShowS
    escapeChar c = showString $ printf "\\u%04X" (fromEnum c)

dateFromTitle :: Item a -> Maybe UTCTime
dateFromTitle item =
  let filePath = toFilePath (itemIdentifier item)
      title = takeBaseName filePath
   in parseTimeM True defaultTimeLocale "%Y-%m-%d" title
        <|> parseTimeM True defaultTimeLocale "%Y-%m" title

-- | True when the title encodes a month (@YYYY-MM@) rather than a date (@YYYY-MM-DD@).
isMonthTitle :: Item a -> Bool
isMonthTitle item =
  let title = takeBaseName (toFilePath (itemIdentifier item))
      asDay = parseTimeM True defaultTimeLocale "%Y-%m-%d" title :: Maybe UTCTime
      asMonth = parseTimeM True defaultTimeLocale "%Y-%m" title :: Maybe UTCTime
   in isNothing asDay && isJust asMonth

-- | 'Hakyll.getItemUTC' supporting @YYYY-MM@ filenames.
itemUTC :: (MonadMetadata m, MonadFail m) => Item a -> m UTCTime
itemUTC item
  | isMonthTitle item, Just t <- dateFromTitle item = return t
  | otherwise = getItemUTC defaultTimeLocale (itemIdentifier item)

-- | 'Hakyll.recentFirst' supporting @YYYY-MM@ filenames.
recentFirstT :: (MonadMetadata m, MonadFail m) => [Item a] -> m [Item a]
recentFirstT items = do
  tagged <- mapM (\i -> (\t -> (i, t)) <$> itemUTC i) items
  return $ map fst $ sortBy (comparing (Down . snd)) tagged

replaceExt :: T.Text -> T.Text -> T.Text -> T.Text
replaceExt oldExt newExt url =
  let (base, fragment) = T.breakOn "#" url
      cleanedBase = if "::" `T.isSuffixOf` base then T.dropEnd 2 base else base
      processedBase =
        if oldExt `T.isSuffixOf` cleanedBase
          then T.replace oldExt newExt cleanedBase
          else cleanedBase
   in processedBase <> fragment

generateId :: T.Text -> T.Text
generateId heading =
  let lower = T.toLower heading
      spaced = T.replace (T.pack " ") (T.pack "-") lower
      filtered = T.filter (\c -> isAlphaNum c || c == '-' || c == '_' || c == '.') spaced
      parts = T.split (== '-') filtered
      nonEmptyParts = filter (not . T.null) parts
      cleaned = if null nonEmptyParts then T.pack "section" else T.intercalate (T.pack "-") nonEmptyParts
   in cleaned

isPublished :: Item a -> Compiler Bool
isPublished item = do
  metadata <- getMetadata (itemIdentifier item)
  case lookupString "published" metadata of
    Just value -> return (value /= "false")
    Nothing -> return (isJust (dateFromTitle item))

isNotDraft :: Item a -> Compiler Bool
isNotDraft item = do
  metadata <- getMetadata (itemIdentifier item)
  return $ isNotDraftMeta metadata

isNotDraftMeta :: Metadata -> Bool
isNotDraftMeta metadata = do
  case lookupString "published" metadata of
    Just value -> value /= "false"
    Nothing -> True
