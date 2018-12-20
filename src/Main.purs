module Main
  ( GeoJSON(..)
  , LineStringJSON
  , PointJSON
  , PolygonJSON
  , parse
  ) where

import Data.Maybe (Maybe)
import Foreign (F, ForeignError(..), fail)
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
type PolygonJSON =
  { type :: String
  , coordinates :: Array (Array (Array Number))
  }

data GeoJSON
  = Point PointJSON
  | LineString LineStringJSON
  | Polygon PolygonJSON

derive instance eqGeoJSON :: Eq GeoJSON

instance readForeignGeoJSON :: ReadForeign GeoJSON where
  readImpl f = do
    o <- readImpl f :: F { type :: String }
    case o.type of
      "Point" -> map Point (readImpl f :: F PointJSON)
      "LineString" -> map LineString (readImpl f :: F LineStringJSON)
      "Polygon" -> map Polygon (readImpl f :: F PolygonJSON)
      _ -> fail (ForeignError "unknown GeoJSON type")

instance showGeoJSON :: Show GeoJSON where
  show (Point r) = "(Point " <> show r <> ")"
  show (LineString r) = "(LineString " <> show r <> ")"
  show (Polygon r) = "(Polygon " <> show r <> ")"

parse :: String -> Maybe GeoJSON
parse = SimpleJSON.readJSON_
