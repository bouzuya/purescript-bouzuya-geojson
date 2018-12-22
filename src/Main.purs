module Main
  ( GeometryObject(..)
  , Position
  ) where

import Foreign (F, ForeignError(..), fail)
import Prelude (class Eq, class Show, bind, map, show, (<>))
import Simple.JSON (class ReadForeign, readImpl)

type Position = Array Number

data GeometryObject
  = Point Position
  | LineString (Array Position)
  | Polygon (Array (Array Position))
  | MultiPoint (Array Position)
  | MultiLineString (Array (Array Position))
  | MultiPolygon (Array (Array (Array Position)))
  | GeometryCollection (Array GeometryObject)

derive instance eqGeometryObject :: Eq GeometryObject

instance readForeignGeometryObject :: ReadForeign GeometryObject where
  readImpl f = do
    o <- readImpl f :: F { type :: String }
    case o.type of
      "Point" ->
        map
          Point
          (map
            _.coordinates
            (readImpl f :: F { coordinates :: Array Number }))
      "LineString" ->
        map
          LineString
          (map
            _.coordinates
            (readImpl f :: F { coordinates :: Array (Array Number) }))
      "Polygon" ->
        map
          Polygon
            (map
              _.coordinates
              (readImpl f :: F { coordinates :: Array (Array (Array Number)) }))
      "MultiPoint" ->
        map
          MultiPoint
            (map
              _.coordinates
              (readImpl f :: F { coordinates :: Array (Array Number) }))
      "MultiLineString" ->
        map
          MultiLineString
          (map
            _.coordinates
            (readImpl f :: F { coordinates :: Array (Array (Array Number)) }))
      "MultiPolygon" ->
        map
          MultiPolygon
          (map
            _.coordinates
            (readImpl f :: F { coordinates :: Array (Array (Array (Array Number))) }))
      "GeometryCollection" ->
        map
          GeometryCollection
          (map
            _.geometries
            (readImpl f :: F { geometries :: Array GeometryObject }))
      _ ->
        fail (ForeignError "unknown GeoJSON type")

instance showGeometryObject :: Show GeometryObject where
  show (Point r) = "(Point " <> show r <> ")"
  show (LineString r) = "(LineString " <> show r <> ")"
  show (Polygon r) = "(Polygon " <> show r <> ")"
  show (MultiPoint r) = "(MultiPoint " <> show r <> ")"
  show (MultiLineString r) = "(MultiLineString " <> show r <> ")"
  show (MultiPolygon r) = "(MultiPolygon " <> show r <> ")"
  show (GeometryCollection r) = "(GeometryCollection " <> show r <> ")"
