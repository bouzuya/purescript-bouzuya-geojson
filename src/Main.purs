module Main
  ( GeometryObject(..)
  , LineStringJSON
  , PointJSON
  , PolygonJSON
  , MultiPointJSON
  , MultiLineStringJSON
  , MultiPolygonJSON
  , GeometryCollectionJSON
  , parse
  ) where

import Data.Maybe (Maybe)
import Foreign (F, ForeignError(..), fail)
import Prelude (class Eq, class Show, bind, map, show, (<>))
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
type MultiPointJSON =
  { type :: String
  , coordinates :: Array (Array Number)
  }
type MultiLineStringJSON =
  { type :: String
  , coordinates :: Array (Array (Array Number))
  }
type MultiPolygonJSON =
  { type :: String
  , coordinates :: Array (Array (Array (Array Number)))
  }
type GeometryCollectionJSON =
  { type :: String
  , geometries :: Array GeometryObject
  }

data GeometryObject
  = Point PointJSON
  | LineString LineStringJSON
  | Polygon PolygonJSON
  | MultiPoint MultiPointJSON
  | MultiLineString MultiLineStringJSON
  | MultiPolygon MultiPolygonJSON
  | GeometryCollection GeometryCollectionJSON

derive instance eqGeometryObject :: Eq GeometryObject

instance readForeignGeometryObject :: ReadForeign GeometryObject where
  readImpl f = do
    o <- readImpl f :: F { type :: String }
    case o.type of
      "Point" -> map Point (readImpl f :: F PointJSON)
      "LineString" -> map LineString (readImpl f :: F LineStringJSON)
      "Polygon" -> map Polygon (readImpl f :: F PolygonJSON)
      "MultiPoint" -> map MultiPoint (readImpl f :: F MultiPointJSON)
      "MultiLineString" -> map MultiLineString (readImpl f :: F MultiLineStringJSON)
      "MultiPolygon" -> map MultiPolygon (readImpl f :: F MultiPolygonJSON)
      "GeometryCollection" -> map GeometryCollection (readImpl f :: F GeometryCollectionJSON)
      _ -> fail (ForeignError "unknown GeoJSON type")

instance showGeometryObject :: Show GeometryObject where
  show (Point r) = "(Point " <> show r <> ")"
  show (LineString r) = "(LineString " <> show r <> ")"
  show (Polygon r) = "(Polygon " <> show r <> ")"
  show (MultiPoint r) = "(MultiPoint " <> show r <> ")"
  show (MultiLineString r) = "(MultiLineString " <> show r <> ")"
  show (MultiPolygon r) = "(MultiPolygon " <> show r <> ")"
  show (GeometryCollection r) = "(GeometryCollection " <> show r <> ")"

parse :: String -> Maybe GeometryObject
parse = SimpleJSON.readJSON_
