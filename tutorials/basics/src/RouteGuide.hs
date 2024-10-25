{-# OPTIONS_GHC -Wno-orphans #-}

module RouteGuide (
    -- * Querying the database
    DB
  , getDB
  , featureAt
  , featuresIn
  , summary
    -- * Route chat
  , Chat
  , emptyChat
  , getNotes
  , recordNote
  ) where

import Data.Aeson
import Data.Int
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Time

import Network.GRPC.Common.Protobuf

import Proto.RouteGuide

import Paths_basics_tutorial

{-------------------------------------------------------------------------------
  Querying the database
-------------------------------------------------------------------------------}

-- | The DB of predefined features
type DB = [Proto Feature]

-- | Load the database from disk
getDB :: IO DB
getDB = do
    path <- getDataFileName "route_guide_db.json"
    either error return =<< eitherDecodeFileStrict path

featureAt :: DB -> Proto Point -> Maybe (Proto Feature)
featureAt db p = listToMaybe $ filter (\f -> f ^. #location == p) db

featuresIn :: DB -> Proto Rectangle -> [Proto Feature]
featuresIn db r = filter (\f -> inRectangle r (f ^. #location)) db

summary :: DB -> NominalDiffTime -> [Proto Point] -> Proto RouteSummary
summary db duration ps =
    defMessage
      & #pointCount   .~ fromIntegral (length ps)
      & #featureCount .~ fromIntegral (length visited)
      & #distance     .~ floor (distance ps)
      & #elapsedTime  .~ round duration
  where
    visited :: [Proto Feature]
    visited = filter (\f -> any (== f ^. #location) ps) db

{-------------------------------------------------------------------------------
  Route chat
-------------------------------------------------------------------------------}

type Chat = Map (Proto Point) [Proto RouteNote]

emptyChat :: Chat
emptyChat = Map.empty

getNotes :: Chat -> Proto Point -> [Proto RouteNote]
getNotes chat p = Map.findWithDefault [] p chat

recordNote :: Proto RouteNote -> Chat -> Chat
recordNote note chat =
    Map.alter (Just . (note :) . fromMaybe []) (note ^. #location) chat

{-------------------------------------------------------------------------------
  Internal Auxiliary
-------------------------------------------------------------------------------}

inRectangle :: Proto Rectangle -> Proto Point -> Bool
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

-- | Total distance between the points
distance :: [Proto Point] -> Double
distance = \case
    []   -> 0
    p:ps -> go 0 p ps
  where
    go :: Double -> Proto Point -> [Proto Point] -> Double
    go !acc _    []     = acc
    go !acc prev (p:ps) = go (acc + distanceBetween prev p) p ps

-- | Distance between two points (in meters)
--
-- For consistency, this is a direct translation of the Python example code in
-- the gRPC repo.
distanceBetween :: Proto Point -> Proto Point -> Double
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

instance FromJSON (Proto Feature) where
  parseJSON = withObject "Feature" $ \obj -> do
      location <- obj .: "location"
      name     <- obj .: "name"
      return $
        defMessage
          & #location .~ location
          & #name     .~ name

instance FromJSON (Proto Point) where
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
