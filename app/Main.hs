module Main where

import Meur.Builder (buildSite)
import Meur.Config (SiteConfig, cacheDir, loadConfig, outputDir, sitePaths)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- | Load site configuration from site.yaml
loadSiteConfig :: IO SiteConfig
loadSiteConfig = do
  let configFile = "site.yaml"
  exists <- doesFileExist configFile
  if not exists
    then do
      hPutStrLn stderr $ "Error: " ++ configFile ++ " not found"
      hPutStrLn stderr "Please create a site.yaml configuration file"
      exitFailure
    else do
      result <- loadConfig configFile
      case result of
        Left err -> do
          hPutStrLn stderr $ "Error loading configuration: " ++ err
          exitFailure
        Right config -> return config

main :: IO ()
main = do
  config <- loadSiteConfig
  buildSite config
