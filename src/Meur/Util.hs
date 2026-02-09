{-# LANGUAGE OverloadedStrings #-}

module Meur.Util
  ( -- JSON escaping
    escapeString,
    -- Date utilities
    dateFromTitle,
    -- URL utilities
    replaceExt,
    generateId,
    -- Draft/published checking
    isPublished,
    isNotDraft,
    isNotDraftMeta,
  )
where

import Data.Char (isAlphaNum)
import Data.Maybe (isJust)
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
