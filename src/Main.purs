module Main
  ( FeatureId
  , FeatureObject(..)
  , GeometryObject(..)
  , Position
  , Properties
  ) where

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, toMaybe)
import Foreign (F, Foreign, ForeignError(..), fail)
import Foreign as Foreign
import Prelude (class Eq, class Show, bind, identity, map, pure, show, (<>))
import Simple.JSON (class ReadForeign, readImpl)
import Simple.JSON as SimpleJSON

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
        fail (ForeignError "unknown Geometry Object type")

instance showGeometryObject :: Show GeometryObject where
  show (Point r) = "(Point " <> show r <> ")"
  show (LineString r) = "(LineString " <> show r <> ")"
  show (Polygon r) = "(Polygon " <> show r <> ")"
  show (MultiPoint r) = "(MultiPoint " <> show r <> ")"
  show (MultiLineString r) = "(MultiLineString " <> show r <> ")"
  show (MultiPolygon r) = "(MultiPolygon " <> show r <> ")"
  show (GeometryCollection r) = "(GeometryCollection " <> show r <> ")"

type Properties = String

type FeatureId = Either Number String

data FeatureObject
  = Feature
      (Maybe GeometryObject)
      (Maybe Properties)
      (Maybe FeatureId)

derive instance eqFeatureObject :: Eq FeatureObject

instance readForeignFeatureObject :: ReadForeign FeatureObject where
  readImpl f = do
    o <- readImpl f :: F { type :: String }
    case o.type of
      "Feature" -> do
        { geometry, properties, id } <-
          readImpl f ::
            F { geometry :: Nullable GeometryObject
              , properties :: Nullable Foreign
              , id :: Maybe Foreign
              }
        id' <-
          case id of
            Nothing -> pure Nothing
            (Just id') ->
              case Foreign.tagOf id' of
                "Number" -> map Just (map Left (Foreign.readNumber id'))
                "String" -> map Just (map Right (Foreign.readString id'))
                _ -> fail (ForeignError "unknown Feature Object id type")
        pure
          (Feature
            (toMaybe geometry)
            (map SimpleJSON.writeJSON (toMaybe properties))
            id')
      _ ->
        fail (ForeignError "unknown Feature Object type")

instance showFeatureObject :: Show FeatureObject where
  show (Feature g p i)
    = "(Feature"
    <> " geometry: " <> maybe "null" show g
    <> " properties: " <> maybe "null" identity p
    <> (maybe "" (\e -> " id: " <> (either show identity e)) i)
    <> ")"
