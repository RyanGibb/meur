{-# LANGUAGE OverloadedStrings #-}

-- From: https://gitlab.sac-home.org/tema/artem-blog/-/blob/master/BibHakyll.hs
-- A lof of the ideas are taken from:
-- https://github.com/jaspervdj/hakyll-bibtex

module Meur.BibHakyll where

import Control.Applicative ((<|>))
import Data.Binary
import Data.Either (fromRight)
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time (UTCTime (UTCTime), defaultTimeLocale, formatTime, parseTimeM, parseTimeOrError)
import Data.Typeable (Typeable)
import Hakyll
import Meur.Bib
import Text.Pandoc
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Citeproc
import Text.Pandoc.Readers
import Text.Pandoc.Walk (walk)
import qualified Text.Pandoc.Walk as B
import Text.Pandoc.Writers
import Text.Parsec hiding ((<|>))

instance Binary Meur.Bib.Entry where
  put (Entry k v) = do
    put k
    put v

  get = Entry <$> get <*> get

instance Binary Bib where
  put (Bib ty nm items) = do
    put ty
    put nm
    put items

  get = Bib <$> get <*> get <*> get

instance Writable Bib where
  write file item = writeFile file (showBib $ itemBody item)

latexifyPlain :: String -> Either PandocError String
latexifyPlain s = do
  la <- runPure $ readLaTeX def $ T.pack s
  te <- runPure $ writePlain def la
  return $ T.unpack (T.unwords . T.words $ te)

latexifyHtml :: String -> Either PandocError String
latexifyHtml s = do
  la <- runPure $ readLaTeX def $ T.pack s
  te <- runPure $ writeHtml5String def la
  return $ T.unpack (T.strip te)

bibClsWith :: (Pandoc -> PandocIO T.Text) -> String -> Bib -> IO String
bibClsWith writer csl bib = do
  -- workaround https://docs.citationstyles.org/en/stable/specification.html#appendix-vi-links
  let s = showBib $ filterKeys bib ["url", "doi", "eprint"]
  res <- runIO $ do
    doc <-
      B.setMeta (T.pack "citation-style") (T.pack csl)
        <$> readBibLaTeX def (T.pack s)
    processed <- cleanup <$> processCitations doc
    writer processed
  T.unpack <$> handleError res
  where
    cleanup :: Pandoc -> Pandoc
    cleanup p = B.doc $ B.plain $ B.fromList $ B.query extractRightInline p

    extractRightInline :: Inline -> [Inline]
    extractRightInline s@(Span (_, ["csl-right-inline"], _) _) = [s]
    extractRightInline _ = []

bibCls :: String -> Bib -> IO String
bibCls = bibClsWith (writeHtml5String def)

bibClsMarkdown :: String -> Bib -> IO String
bibClsMarkdown = bibClsWith (writeMarkdown def)

bibContext :: String -> String -> Context Bib
bibContext csl dateFormat =
  Context $ \key _ item -> do
    let b = itemBody item
    let name = Meur.Bib.name b
    parsed <- unsafeCompiler $ bibCls csl b
    parsedMarkdown <- unsafeCompiler $ bibClsMarkdown csl b
    let latexifyHtml' = fromRight (error "bibToContext for entry " <> name) . latexifyHtml
    let latexifyPlain' = fromRight (error "bibToContext for entry " <> name) . latexifyPlain
    let str s = return $ StringField s
    let strPlain = str . latexifyPlain' :: String -> Compiler ContextField
    let strHtml = str . latexifyHtml' :: String -> Compiler ContextField
    let lookup key = case bibIndex b key of
          Nothing ->
            noResult $
              "No key " <> key <> " in bibitem " <> name
          Just x -> return x
    case key of
      "name" -> str name
      "entrytype" -> str $ entrytype b
      "parsed" -> str parsed
      "mdParsed" -> str parsedMarkdown
      "abstract" -> strHtml =<< lookup "abstract"
      "mdAbstract" -> strPlain =<< lookup "abstract"
      "bib" -> str (showBib $ filterKeys b ["parsed", "abstract", "addinfo", "date", "video", "slides", "tags"])
      x | x == "date" || x == "published" || x == "updated" -> str (formatTime defaultTimeLocale dateFormat (bibDate b))
      _ -> strPlain =<< lookup key

bibDate :: Bib -> UTCTime
bibDate b =
  let latexifyPlain' = fromRight (error $ "bibDate for entry " <> Meur.Bib.name b) . latexifyPlain

      tryParseDate :: String -> Maybe UTCTime
      tryParseDate dateStr =
        parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr
          <|> parseTimeM True defaultTimeLocale "%Y-%m" dateStr
          <|> parseTimeM True defaultTimeLocale "%Y" dateStr

      tryParseYearMonth :: String -> String -> Maybe UTCTime
      tryParseYearMonth year month =
        parseTimeM True defaultTimeLocale "%Y-%m" (year <> "-" <> month)
          <|> parseTimeM True defaultTimeLocale "%Y-%b" (year <> "-" <> month)
          <|> parseTimeM True defaultTimeLocale "%Y-%B" (year <> "-" <> month)

      getDate :: Maybe UTCTime
      getDate =
        case bibIndex b "date" of
          Just dateField -> tryParseDate (latexifyPlain' dateField)
          Nothing ->
            case bibIndex b "year" of
              Just yearField ->
                let year = latexifyPlain' yearField
                 in case bibIndex b "month" of
                      Just monthField -> tryParseYearMonth year (latexifyPlain' monthField)
                      Nothing -> tryParseDate year
              Nothing -> Nothing
   in case getDate of
        Nothing -> error $ "bibDate: no valid date found in entry " <> Meur.Bib.name b
        Just parsedDate -> parsedDate

newtype Bibs = Bibs [Bib]
  deriving (Show, Typeable)

instance Binary Bibs where
  put (Bibs b) = put b
  get = Bibs <$> get

instance Writable Bibs where
  write file item =
    let Bibs bs = itemBody item
     in writeFile file $ showBibs bs

parseBibFile s = case parse parseBibs "" s of
  Right p -> Bibs p
  Left p -> error $ "Parse error: " <> show p

bibFileCompiler :: Compiler (Item Bibs)
bibFileCompiler = do
  fmap parseBibFile <$> getResourceString
