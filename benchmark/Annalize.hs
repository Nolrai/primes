{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Annalize
  ( annalize,
  )
where

import Criterion.Types as C
import Data.Aeson
import Data.Aeson.Types
import qualified Data.List as List
import Data.Map as Map
import Data.Vector as V
-- import Graphics.Gnuplot.Simple
import Statistics.Types

data RecordFile
  = RecordFile
      { rf_library :: String,
        rf_version :: String,
        rf_reports :: V.Vector Report
      }
  deriving stock (Show)

instance FromJSON RecordFile where
  parseJSON = withArray "RecordFile" $
    \v ->
      do
        Just rf_library <- parseJSON (v V.! 0)
        Just rf_version <- parseJSON (v V.! 1)
        rf_reports <- withArray "Report Array" parseReports (v V.! 2)
        pure RecordFile {..}

parseReports :: Array -> Parser (V.Vector Report)
parseReports v =
  V.mapM parseJSON v

annalize :: FilePath -> IO any
annalize path =
  do
    (rf_ :: Either String RecordFile) <- eitherDecodeFileStrict path
    case rf_ of
      Left str -> die str
      Right rf ->
        do
          let table = toTable (rf_reports rf)
          -- plotList [] table
          die $ show table

toTable :: Vector Report -> [(Int, Int, Int, Int)]
toTable =
  fromMap . splitIntoRuns . changeUnits . toMap . fmap toTuple

fromMap ::
  forall a c.
  (Show c) =>
  Map a (Map String c) ->
  [(a, c, c, c)]
fromMap = fmap to4tuple . Map.toList . fmap toTripple

to4tuple :: (a, (c, c, c)) -> (a, c, c, c)
to4tuple = \(x, (y, z, w)) -> (x, y, z, w)

toTripple :: (Show c) => Map String c -> (c, c, c)
toTripple m =
  case Map.toList m of
    [("fakeSieve", fake), ("naive", naive), ("realSieve", real)] ->
      (naive, fake, real) --change so naive is first
    _ -> error $ "toTripple recived " <> show (Map.toList m)

splitIntoRuns ::
  forall a b c.
  (Ord a, Ord b, Show a, Show b, Show c) =>
  Map (a, b) c ->
  Map a (Map b c)
splitIntoRuns oldMap =
  Map.foldlWithKey'
    addItemToSubMap
    mempty
    oldMap

addItemToSubMap ::
  forall a b c.
  (Ord a, Ord b, Show a, Show b, Show c) =>
  Map a (Map b c) ->
  (a, b) ->
  c ->
  Map a (Map b c)
addItemToSubMap soFar (a, b) c =
  insertWith
    (unionWithKey err)
    a
    (Map.singleton b c)
    soFar
  where
    err :: forall t. b -> c -> c -> t
    err name _one _two =
      error $
        "trying to overwrite " <> show a
          <> " in column "
          <> show name

toLogLog :: (Floating a) => Map Int a -> Map Float a
toLogLog =
  mapKeysMonotonic (logBase 10 . fromIntegral)
    . fmap (logBase 10)

changeUnits :: Map k Double -> Map k Int
changeUnits m = (floor . (* 10 ^ (9 :: Int))) <$> m

toMap :: (Ord a, Ord b) => V.Vector (a, b, c) -> Map (a, b) c
toMap = Map.fromList . V.toList . fmap (\(a, b, c) -> ((a, b), c))

toTuple :: Report -> (Int, String, Double)
toTuple Report {..} =
  case reads (List.drop 1 n') of
    [(n, "")] -> (n, name, mean)
    _ -> error $ "n' is " <> show n'
  where
    (name, n') = List.break (== '/') reportName
    mean = estPoint . anMean $ reportAnalysis
