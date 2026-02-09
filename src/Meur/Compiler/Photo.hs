{-# LANGUAGE OverloadedStrings #-}

module Meur.Compiler.Photo
  ( -- EXIF utilities
    formatNumeric,
    formatAsNumber,
    ppExposureTime,
    exifMetadata,
    -- EXIF fields
    exifField,
    exifLatField,
    exifLongField,
    exifDateField,
    -- GPS utilities
    decodeGpsCoordinate,
    getLatitude,
    getLongitude,
    -- Location
    readCachedLocation,
    formatLocation,
    locationField,
    -- Other photo fields
    thumbnailField,
    photoContext,
  )
where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import Data.Either (fromRight)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import qualified Data.Text as T
import Data.Time.Format (formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Graphics.HsExif
import Hakyll
import Meur.Types (PhotoLocation (..))
import Numeric (showFFloat)
import System.Directory (doesFileExist)
import System.FilePath (takeFileName, (</>))

-- https://github.com/emmanueltouzery/hsexif/issues/23#issuecomment-2835135828
formatNumeric :: (Int -> Int -> ShowS) -> ExifValue -> String
formatNumeric f (ExifRational num den) = f num den ""
formatNumeric f (ExifRationalList values) = go values ""
  where
    go [] = id
    go [(n, d)] = f n d
    go ((n, d) : ns) = f n d . showString ", " . go ns
formatNumeric _ value = show value

formatAsNumber :: Int -> ExifValue -> String
formatAsNumber n = formatNumeric fmt
  where
    fmt num den s = trim0 (fltString num den) ++ s
    trim0 = reverse . dropWhile ('.' ==) . dropWhile ('0' ==) . reverse
    fltString num den = showFFloat (Just n) (fromIntegral num / fromIntegral den :: Double) ""

ppExposureTime :: ExifValue -> String
ppExposureTime v@(ExifRational num den) =
  let seconds = fromIntegral num / (fromIntegral den :: Double)
      value
        | seconds <= 0.25 && seconds > 0 = "1/" ++ show (round (1 / seconds) :: Int)
        | otherwise = formatAsNumber 1 v
   in T.unpack $ T.append (T.pack value) " sec."
ppExposureTime v = show v

-- TODO don't load metadata individually for each field
exifMetadata :: Item a -> Compiler (M.Map ExifTag ExifValue)
exifMetadata item = do
  let identifier = itemIdentifier item
  exifData <- unsafeCompiler (parseFileExif (toFilePath identifier))
  return $ fromRight M.empty exifData

exifField :: String -> ExifTag -> (ExifValue -> String) -> Context a
exifField key tag print =
  field key $ \item -> do
    metadata <- exifMetadata item
    case M.lookup tag metadata of
      Nothing -> noResult ""
      Just value -> return $ print value

exifLatField :: String -> Context a
exifLatField key =
  field key $ \item -> do
    metadata <- exifMetadata item
    case getLatitude metadata of
      Nothing -> noResult ""
      Just lat -> return $ show lat

exifLongField :: String -> Context a
exifLongField key =
  field key $ \item -> do
    metadata <- exifMetadata item
    case getLongitude metadata of
      Nothing -> noResult ""
      Just lon -> return $ show lon

decodeGpsCoordinate :: ExifValue -> Maybe Double
decodeGpsCoordinate (ExifRationalList intPairs) =
  case fmap intPairToFloating intPairs of
    [degrees, minutes, seconds] -> Just (degrees + minutes / 60 + seconds / 3600)
    _ -> Nothing
  where
    intPairToFloating (n, d) = fromIntegral n / fromIntegral d
decodeGpsCoordinate _ = Nothing

getLatitude :: M.Map ExifTag ExifValue -> Maybe Double
getLatitude exifData = do
  latDec <- M.lookup gpsLatitude exifData >>= decodeGpsCoordinate
  let latRef = case M.lookup gpsLatitudeRef exifData of
        Just (ExifText ref) -> ref
        _ -> "N"
  return $ if latRef == "S" then -latDec else latDec

getLongitude :: M.Map ExifTag ExifValue -> Maybe Double
getLongitude exifData = do
  longDec <- M.lookup gpsLongitude exifData >>= decodeGpsCoordinate
  let longRef = case M.lookup gpsLongitudeRef exifData of
        Just (ExifText ref) -> ref
        _ -> "E"
  return $ if longRef == "W" then -longDec else longDec

exifDateField :: String -> String -> Context a
exifDateField key format =
  field key $ \item -> do
    metadata <- exifMetadata item
    case getDateTimeOriginal metadata of
      Nothing -> noResult ""
      Just date -> return $ formatTime defaultTimeLocale format date

-- | Read cached location from geocoding cache directory
-- Takes an optional cache directory path; if Nothing, geocoding is disabled
readCachedLocation :: Maybe FilePath -> FilePath -> IO (Either String PhotoLocation)
readCachedLocation maybeCacheDir photoPath = do
  case maybeCacheDir of
    Nothing -> return $ Left "Geocoding cache disabled"
    Just cacheDir -> do
      let cacheFile = cacheDir </> takeFileName photoPath ++ ".json"
      exists <- doesFileExist cacheFile
      if not exists
        then return $ Left "Cache file not found"
        else eitherDecode <$> BSL.readFile cacheFile

formatLocation :: HM.HashMap T.Text T.Text -> T.Text
formatLocation m =
  let country = HM.lookup "country" m
      city = HM.lookup "city" m
      state_district = HM.lookup "state_district" m
      heirarchy
        | country == Just "United States" && city == Just "New York" =
            [ ["borough"],
              ["state"],
              ["country"]
            ]
        | country == Just "United States" =
            [ ["city", "town", "village", "road"],
              ["state"],
              ["country"]
            ]
        | country == Just "United Kingdom" && city == Just "London" =
            [ ["suburb"],
              ["city"],
              ["country"]
            ]
        | country == Just "United Kingdom" && state_district == Just "Greater London" =
            [ ["city"],
              ["state_district"],
              ["country"]
            ]
        | country == Just "United Kingdom" =
            [ ["city", "town", "village"],
              ["country"]
            ]
        | country == Just "France" && city == Just "Paris" =
            [ ["suburb"],
              ["city"],
              ["country"]
            ]
        | country == Just "Italy" =
            [ ["quarter"],
              ["city", "town", "village"],
              ["state"],
              ["country"]
            ]
        | otherwise =
            [ ["historic"],
              ["city", "state", "region", "town"],
              ["country"]
            ]
      lookupFirst ks = listToMaybe $ mapMaybe (`HM.lookup` m) ks
      fields = map lookupFirst heirarchy
   in T.intercalate ", " (catMaybes fields)

-- | Location field that uses cached geocoding data
-- The cache directory should be passed via the context creation
locationField :: Maybe FilePath -> String -> Context a
locationField maybeCacheDir key = field key $ \item -> do
  let fp = toFilePath (itemIdentifier item)
  eLoc <- unsafeCompiler $ readCachedLocation maybeCacheDir fp
  case eLoc of
    Left _ -> noResult ""
    Right loc ->
      let txt = formatLocation (addressMap loc)
       in if T.null txt then noResult "" else return (T.unpack txt)

thumbnailField :: String -> Context a
thumbnailField key = field key $ \item -> do
  mRoute <- getRoute (itemIdentifier item)
  case mRoute of
    Nothing -> noResult ""
    Just url ->
      if ".mp4" `L.isSuffixOf` url
        then noResult ""
        else
          return $
            T.unpack $
              T.replace "photos/" "photos/thumb/" (T.pack url)

-- | Create a photo context with optional geocoding cache directory
photoContext :: Maybe FilePath -> String -> Context a
photoContext maybeCacheDir dateFormat =
  dateField "published" dateFormat
    `mappend` dateField "updated" dateFormat
    `mappend` urlField "url"
    `mappend` pathField "path"
    `mappend` titleField "title"
    `mappend` thumbnailField "thumb"
    `mappend` exifDateField "published" dateFormat
    `mappend` exifDateField "updated" dateFormat
    `mappend` exifLatField "lat"
    `mappend` exifLongField "lon"
    `mappend` exifField "make" make show
    `mappend` exifField "model" model show
    `mappend` exifField "focallength" focalLength (formatAsFloatingPoint 2)
    `mappend` exifField "aperture" apertureValue (formatAsFloatingPoint 2)
    `mappend` exifField "exposure" exposureTime ppExposureTime
    `mappend` exifField "iso" isoSpeedRatings show
    `mappend` locationField maybeCacheDir "loc"
