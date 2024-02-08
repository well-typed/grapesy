{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Demo.Server.Aux.RouteGuide (
    featureAt
  , inRectangle
  , summary
  , distance
  ) where

import Data.Aeson
import Data.Int
import Data.Maybe (listToMaybe)
import Data.Time

import Proto.RouteGuide

import Network.GRPC.Common.Protobuf

{-------------------------------------------------------------------------------
  Pure functions that implement that basic Route Guide functionality
-------------------------------------------------------------------------------}

featureAt :: [Feature] -> Point -> Maybe Feature
featureAt db p = listToMaybe $ filter (\f -> f ^. #location == p) db

inRectangle :: Rectangle -> Point -> Bool
inRectangle r p = and [
      p ^. #longitude >= left
    , p ^. #longitude <= right
    , p ^. #latitude  >= bottom
    , p ^. #latitude  <= top
    ]
  where
   left, right, top, bottom :: Int32
   left   = min (r ^. #lo ^. #longitude) (r ^. #hi ^. #longitude)
   right  = max (r ^. #lo ^. #longitude) (r ^. #hi ^. #longitude)
   top    = max (r ^. #lo ^. #latitude)  (r ^. #hi ^. #latitude)
   bottom = min (r ^. #lo ^. #latitude)  (r ^. #hi ^. #latitude)

summary :: [Feature] -> NominalDiffTime -> [Point] -> RouteSummary
summary db duration ps =
    defMessage
      & #pointCount   .~ fromIntegral (length ps)
      & #featureCount .~ fromIntegral (length visited)
      & #distance     .~ floor (distance ps)
      & #elapsedTime  .~ round duration
  where
    visited :: [Feature]
    visited = filter (\f -> any (== f ^. #location) ps) db

-- | Total distance between the points
distance :: [Point] -> Double
distance = \case
    []   -> 0
    p:ps -> go 0 p ps
  where
    go :: Double -> Point -> [Point] -> Double
    go !acc _    []     = acc
    go !acc prev (p:ps) = go (acc + distanceBetween prev p) p ps

-- | Distance between two points (in meters)
--
-- For consistency, this is a direct translation of the Python example code in
-- the gRPC repo.
distanceBetween :: Point -> Point -> Double
distanceBetween fr to =
    let a, c :: Double
        a = sin (deltaLat / 2) ** 2
          + (cos frLat * cos toLat * sin (deltaLon / 2) ** 2)
        c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    in r * c
  where
    coordFactor :: Double
    coordFactor = 10_000_000

    frLat, frLon, toLat, toLon :: Double
    frLat = degToRad $ fromIntegral (fr ^. #latitude)  / coordFactor
    frLon = degToRad $ fromIntegral (fr ^. #longitude) / coordFactor
    toLat = degToRad $ fromIntegral (to ^. #latitude)  / coordFactor
    toLon = degToRad $ fromIntegral (to ^. #longitude) / coordFactor

    deltaLat, deltaLon :: Double
    deltaLat = toLat - frLat
    deltaLon = toLon - frLon

    -- Earth's radius
    r :: Double
    r = 6371000

{-------------------------------------------------------------------------------
  JSON
-------------------------------------------------------------------------------}

instance FromJSON Feature where
  parseJSON = withObject "Feature" $ \obj -> do
      location <- obj .: "location"
      name     <- obj .: "name"
      return $
        defMessage
          & #location .~ location
          & #name     .~ name

instance FromJSON Point where
  parseJSON = withObject "Point" $ \obj -> do
      latitude  <- obj .: "latitude"
      longitude <- obj .: "longitude"
      return $
        defMessage
          & #latitude  .~ latitude
          & #longitude .~ longitude

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

degToRad :: Double -> Double
degToRad d = d * (pi / 180)
