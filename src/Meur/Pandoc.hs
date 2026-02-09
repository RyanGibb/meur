{-# LANGUAGE OverloadedStrings #-}

module Meur.Pandoc
  ( readerOptions,
    writerOptions,
    markdownWriterOptions,
    bibRenderHtml,
    bibRenderMarkdown,
    bibRenderFeed,
    pandocTransform,
    pandocTransformFeed,
    rewriteLinksInPandoc,
    rewriteLinkUrl,
    replaceForHtml,
    replaceForMarkdown,
    extractCitationKeys,
  )
where

import Control.Monad ((>=>))
import qualified Data.Map as M
import qualified Data.Text as T
import Hakyll
import Meur.Config (PathConfig, luaFilters, luaFiltersFeed, scriptsDir)
import Meur.Util (generateId, replaceExt)
import System.FilePath ((</>))
import Text.Pandoc
import Text.Pandoc.Highlighting (pygments)
import Text.Pandoc.Lua (applyFilter)
import Text.Pandoc.Walk (walk)

readerOptions :: ReaderOptions
readerOptions =
  def
    { readerExtensions = foldr enableExtension pandocExtensions [Ext_citations, Ext_smart]
    }

writerOptions :: WriterOptions
writerOptions =
  def
    { writerExtensions = enableExtension Ext_smart pandocExtensions,
      writerHighlightStyle = Just pygments,
      writerCiteMethod = Citeproc
    }

markdownWriterOptions :: WriterOptions
markdownWriterOptions =
  def
    { writerExtensions = extensionsFromList [Ext_smart],
      writerHighlightStyle = Just pygments,
      writerCiteMethod = Citeproc
    }

bibRenderHtml :: PathConfig -> String -> String -> Item String -> Compiler (Item String)
bibRenderHtml pathConfig cslFileName bibFileName pandoc = do
  csl <- load $ fromFilePath cslFileName
  bib <- load $ fromFilePath bibFileName
  let transform =
        withItemBody
          ( \(Pandoc (Meta meta) bs) ->
              pure $
                Pandoc
                  (Meta $ M.insert "link-citations" (MetaBool True) meta)
                  bs
          )
          >=> processPandocBiblios csl [bib]
          >=> withItemBody (pandocTransform (scriptsDir pathConfig) (luaFilters pathConfig))
          >=> withItemBody (pure . rewriteLinksInPandoc replaceForHtml)
  renderPandocItemWithTransformM readerOptions writerOptions transform pandoc

bibRenderMarkdown :: PathConfig -> String -> String -> Item String -> Compiler (Item String)
bibRenderMarkdown pathConfig cslFileName bibFileName pandoc = do
  csl <- load $ fromFilePath cslFileName
  bib <- load $ fromFilePath bibFileName
  let transform =
        withItemBody
          ( \(Pandoc (Meta meta) bs) ->
              pure $
                Pandoc
                  (Meta $ M.insert "link-citations" (MetaBool True) meta)
                  bs
          )
          >=> processPandocBiblios csl [bib]
          >=> withItemBody (pure . rewriteLinksInPandoc replaceForMarkdown)
  ( \(Item itemi doc) ->
      case runPure $ writeMarkdown markdownWriterOptions doc of
        Left err -> error $ "Hakyll.Web.Pandoc.writePandocWith: " ++ show err
        Right item' -> Item itemi $ T.unpack item'
    )
    <$> (transform =<< readPandocWith readerOptions pandoc)

bibRenderFeed :: PathConfig -> String -> String -> Item String -> Compiler (Item String)
bibRenderFeed pathConfig cslFileName bibFileName pandoc = do
  csl <- load $ fromFilePath cslFileName
  bib <- load $ fromFilePath bibFileName
  let transform =
        withItemBody
          ( \(Pandoc (Meta meta) bs) ->
              pure $
                Pandoc
                  (Meta $ M.insert "link-citations" (MetaBool True) meta)
                  bs
          )
          >=> processPandocBiblios csl [bib]
          >=> withItemBody (pandocTransformFeed (scriptsDir pathConfig) (luaFiltersFeed pathConfig))
          >=> withItemBody (pure . rewriteLinksInPandoc replaceForHtml)
  renderPandocItemWithTransformM readerOptions writerOptions transform pandoc

-- | Apply Pandoc Lua filters from the scripts directory
pandocTransform :: FilePath -> [FilePath] -> Pandoc -> Compiler Pandoc
pandocTransform scriptsDir filters =
  unsafeCompiler
    . runIOorExplode
    . applyFilters scriptsDir filters

pandocTransformFeed :: FilePath -> [FilePath] -> Pandoc -> Compiler Pandoc
pandocTransformFeed scriptsDir filters =
  unsafeCompiler
    . runIOorExplode
    . applyFilters scriptsDir filters

-- | Chain multiple Lua filters together
applyFilters :: FilePath -> [FilePath] -> Pandoc -> PandocIO Pandoc
applyFilters scriptsDir filters = foldr (>=>) return $ map (\f -> applyFilter def [] (scriptsDir </> f)) filters

rewriteLinksInPandoc :: (T.Text -> T.Text) -> Pandoc -> Pandoc
rewriteLinksInPandoc replacer = walk rewriteLink
  where
    rewriteLink :: Inline -> Inline
    rewriteLink (Link attr inlines (url, title)) =
      Link attr inlines (rewriteLinkUrl replacer url, title)
    rewriteLink x = x

rewriteLinkUrl :: (T.Text -> T.Text) -> T.Text -> T.Text
rewriteLinkUrl replace url
  | "://" `T.isInfixOf` url = url
  | "mailto:" `T.isPrefixOf` url = url
  | "::" `T.isInfixOf` url =
      let (basePart, rest) = T.breakOn "::" url
          cleanedBase = replace basePart
          headingPart = T.drop 2 rest
          generatedId = generateId headingPart
       in cleanedBase <> "#" <> generatedId
  | otherwise =
      let (base, fragment) = T.breakOn "#" url
          processedBase = replace base
       in processedBase <> fragment

replaceForHtml :: T.Text -> T.Text
replaceForHtml = replaceExt ".md" ".html" . replaceExt ".org" ".html"

replaceForMarkdown :: T.Text -> T.Text
replaceForMarkdown = replaceExt ".html" ".md" . replaceExt ".org" ".md"

extractCitationKeys :: Pandoc -> [T.Text]
extractCitationKeys (Pandoc _ blocks) =
  let extractFromInline (Cite citations _) = map citationId citations
      extractFromInline _ = []
      extractFromBlock (Para inlines) = concatMap extractFromInline inlines
      extractFromBlock (Plain inlines) = concatMap extractFromInline inlines
      extractFromBlock (BlockQuote blocks') = concatMap extractFromBlock blocks'
      extractFromBlock (OrderedList _ blockLists) = concatMap (concatMap extractFromBlock) blockLists
      extractFromBlock (BulletList blockLists) = concatMap (concatMap extractFromBlock) blockLists
      extractFromBlock (DefinitionList items) = concatMap (\(inlines, blockLists) -> concatMap extractFromInline inlines ++ concatMap (concatMap extractFromBlock) blockLists) items
      extractFromBlock (Header _ _ inlines) = concatMap extractFromInline inlines
      extractFromBlock (Div _ blocks') = concatMap extractFromBlock blocks'
      extractFromBlock _ = []
   in concatMap extractFromBlock blocks
