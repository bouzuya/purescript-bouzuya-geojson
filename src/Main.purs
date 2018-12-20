module Main
  ( GeoJSON(..)
  , LineStringJSON
  , PointJSON
  , parse
  ) where

import Data.Maybe (Maybe)
import Foreign (F)
import Prelude (class Eq, class Show, bind, map, show, (<>), (==))
import Simple.JSON (class ReadForeign, readImpl)
import Simple.JSON as SimpleJSON

type PointJSON =
  { type :: String
  , coordinates :: Array Number
  }
type LineStringJSON =
  { type :: String
  , coordinates :: Array (Array Number)
  }

data GeoJSON
  = Point PointJSON
  | LineString LineStringJSON

derive instance eqGeoJSON :: Eq GeoJSON

instance readForeignGeoJSON :: ReadForeign GeoJSON where
  readImpl f = do
    o <- readImpl f :: F { type :: String }
    if o.type == "Point"
      then map Point (readImpl f :: F PointJSON)
      else map LineString (readImpl f :: F LineStringJSON)

instance showGeoJSON :: Show GeoJSON where
  show (Point r) = "(Point " <> show r <> ")"
  show (LineString r) = "(LineString " <> show r <> ")"

parse :: String -> Maybe GeoJSON
parse = SimpleJSON.readJSON_
