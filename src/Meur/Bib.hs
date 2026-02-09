{-# LANGUAGE CPP #-}

-- From: https://gitlab.sac-home.org/tema/artem-blog/-/blob/master/Bib.hs

module Meur.Bib where

import Data.List
import Data.Text (pack, toLower, unpack)
import Data.Typeable
import Text.Parsec
import Text.Parsec.String

data Entry = Entry
  { key :: String,
    value :: String
  }
  deriving (Show, Typeable)

-- Not trying to be smart in understanding
-- semantics of bib files.
data Bib = Bib
  { entrytype :: String,
    name :: String,
    entries :: [Entry]
  }
  deriving (Show, Typeable)

-- Normalise key to lowercase for now.
normaliseKey :: String -> String
normaliseKey = unpack . toLower . pack

showEntry :: Entry -> String
showEntry (Entry k v) = k <> " = " <> v

showBib :: Bib -> String
showBib (Bib b tag ents) =
  "@"
    <> b
    <> "{"
    <> tag
    <> ",\n"
    <> intercalate ",\n" (fmap (("  " <>) . showEntry) ents)
    <> "\n}"

showBibs :: [Bib] -> String
showBibs = intercalate "\n\n" . fmap showBib

-- Values have no structure, simply well-braced
-- strings that we concat together.  This can surely
-- improve if we want to validate whether any of
-- these are correct.
parseVal :: Bool -> Parser String
parseVal b = (("{" <>) . (<> "}") <$> recurse) <|> terminal b
  where
    terminal :: Bool -> Parser String
    terminal True = many1 (noneOf "{}")
    terminal False = many1 (noneOf "{},")

    recurse =
      between
        (char '{')
        (char '}')
        (concat <$> many (parseVal True))

parseComment :: Parser ()
parseComment = do
  try (string "/*")
  rest
  where
    rest :: Parser ()
    rest =
      do
        (try (string "*/") >> return ())
        <|> (char '*' >> rest)
        <|> (anyChar >> rest)

ws = spaces >> skipMany (parseComment >> spaces)

-- Make the key lowercase, just to normalise it to something.
-- Can choose any other normalform later.
parseEntry :: Parser Entry
parseEntry = do
  ws
  k <- (:) <$> letter <*> many (alphaNum <|> char '_')
  ws >> char '=' >> ws
  -- _ <- char '=' <* spaces
  v <- concat <$> many1 (parseVal False) -- <* char ','
  return $ Entry (normaliseKey k) v

parseBib :: Parser Bib
parseBib = do
  bib <- char '@' *> manyTill anyChar (char '{')
  name <- manyTill anyChar (char ',')
  spaces
  ents <- parseEntry `sepEndBy` (char ',' <* ws)
  char '}'
  ws
  return $ Bib bib (normaliseKey name) ents

parseBibs :: Parser [Bib]
parseBibs = ws *> many (parseBib <* ws) <* eof

loadBibs :: FilePath -> IO (Either String [Bib])
loadBibs path = do
  f <- readFile path
  let r = parse parseBibs "" f
  return $ case r of
    Right p -> Right p
    Left p -> Left $ "Parse error: " <> show p

bibIndex :: Bib -> String -> Maybe String
bibIndex (Bib ty name items) kk = value <$> safeHead filtered
  where
    safeHead [] = Nothing
    safeHead (x : _) = Just x
    filtered = filter (\e -> key e == normaliseKey kk) items

filterKeys :: Bib -> [String] -> Bib
filterKeys (Bib ty name items) keys = Bib ty name filtered
  where
    filtered = filter (\e -> key e `notElem` keys) items

filterKey :: Bib -> String -> Bib
filterKey b kk = filterKeys b [kk]

mapEntriesIfKey :: (String -> Bool) -> (String -> String) -> [Entry] -> [Entry]
mapEntriesIfKey p f = fmap (\e@(Entry k v) -> if p k then Entry k (f v) else e)

bibsIndex :: [Bib] -> String -> Maybe Bib
bibsIndex [] _ = Nothing
bibsIndex (b : xs) k | normaliseKey k == name b = return b
bibsIndex (b : xs) k = bibsIndex xs k

#if 0
main = do
  t <- readFile "papers.bib"
  let r = parse parseBibs "" t
  writeFile "out" $ case r of
    Right p -> showBibs p
    Left p -> "Parse error: " <> show p
#endif
