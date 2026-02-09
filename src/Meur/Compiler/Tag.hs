{-# LANGUAGE OverloadedStrings #-}

module Meur.Compiler.Tag
  ( buildBibTags,
    mergeTags,
    getBibTags,
    getBibTitle,
    bibHasTag,
    bibKindPlural,
    bibKindSingular,
    tagField,
    tagContext,
    pageTagsField,
    bibTagsField,
  )
where

import Data.Either (fromRight)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Hakyll
import Meur.Bib (Bib, bibIndex, name)
import qualified Meur.Bib
import Meur.BibHakyll (latexifyPlain)
import Meur.Types (BibKind (..))
import Meur.Util (escapeString)

bibKindPlural :: BibKind -> String
bibKindPlural kind = case kind of
  Paper -> "papers"
  Talk -> "talks"

bibKindSingular :: BibKind -> String
bibKindSingular kind = case kind of
  Paper -> "paper"
  Talk -> "talk"

buildBibTags :: [(BibKind, Bib)] -> (String -> Identifier) -> Tags
buildBibTags bibs makeId =
  let tagMap =
        M.toList $
          foldl
            addBibTags
            M.empty
            [(bibKindPlural kind, bib) | (kind, bib) <- bibs]
   in Tags tagMap makeId (PatternDependency mempty S.empty)
  where
    addBibTags tagMap (prefix, bib) =
      let bibId = fromFilePath $ prefix ++ "/" ++ Meur.Bib.name bib ++ ".html"
          tags' = getBibTags bib
          tagMap' = M.fromList $ zip tags' $ repeat [bibId]
       in M.unionWith (++) tagMap tagMap'

mergeTags :: Tags -> Tags -> Tags
mergeTags t1 t2 =
  Tags
    { tagsMap = M.toList $ M.unionWith (++) (M.fromList $ tagsMap t1) (M.fromList $ tagsMap t2),
      tagsMakeId = tagsMakeId t1,
      tagsDependency = tagsDependency t1
    }

getBibTags :: Bib -> [String]
getBibTags bib = case bibIndex bib "tags" of
  Nothing -> []
  Just tagsStr -> map (T.unpack . T.strip) $ T.splitOn "," $ T.pack $ (fromRight (error "getBibTags for entry " <> Meur.Bib.name bib) $ latexifyPlain tagsStr)

getBibTitle :: Bib -> String
getBibTitle bib = do
  let n = Meur.Bib.name bib
  maybe n (fromRight (error "bibToContext for entry " <> n) . latexifyPlain) (bibIndex bib "title")

bibHasTag :: String -> Bib -> Bool
bibHasTag tag bib = tag `elem` getBibTags bib

tagField :: Context String
tagField =
  field "tag" (return . itemBody)
    `mappend` field "lowerTag" (return . T.unpack . T.toLower . T.pack . itemBody)
    `mappend` field "escapedTag" (return . escapeString . itemBody)

tagContext :: Tags -> Context (String, [Identifier])
tagContext tags =
  Context $ \k _ i ->
    let (tag, identifiers) = itemBody i
     in case k of
          "tag" -> return $ StringField tag
          "url" ->
            let tagId = tagsMakeId tags tag
                url = toUrl $ toFilePath tagId
             in return $ StringField url
          "count" -> return $ StringField $ show $ length identifiers
          _ -> noResult $ "tagContext: unknown key " ++ k

pageTagsField :: String -> Context String
pageTagsField key =
  listFieldWith key tagField $ \item -> do
    tags' <- getTags (itemIdentifier item)
    case tags' of
      [] -> noResult ""
      ts -> mapM makeItem ts

bibTagsField :: String -> Context Bib
bibTagsField key =
  listFieldWith key tagField $ \item ->
    case getBibTags (itemBody item) of
      [] -> noResult ""
      ts -> mapM makeItem ts
