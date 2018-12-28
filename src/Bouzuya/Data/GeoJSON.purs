module Bouzuya.Data.GeoJSON
  ( FeatureCollectionObject(..)
  , FeatureId
  , FeatureObject(..)
  , GeoJSON(..)
  , GeometryObject(..)
  , Position
  , Properties
  ) where

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Foreign (F, Foreign, ForeignError(..))
import Foreign as Foreign
import Prelude (class Eq, class Show, bind, identity, map, pure, show, (<<<), (<>), (>>=))
import Simple.JSON (class ReadForeign, class WriteForeign)
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
    o <- SimpleJSON.readImpl f :: F { type :: String }
    case o.type of
      "Point" ->
        map
          (Point <<< _.coordinates)
          (SimpleJSON.readImpl f :: F { coordinates :: Position })
      "LineString" ->
        map
          (LineString <<< _.coordinates)
          (SimpleJSON.readImpl f :: F { coordinates :: Array Position })
      "Polygon" ->
        map
          (Polygon <<< _.coordinates)
          (SimpleJSON.readImpl f :: F { coordinates :: Array (Array Position) })
      "MultiPoint" ->
        map
          (MultiPoint <<< _.coordinates)
          (SimpleJSON.readImpl f :: F { coordinates :: Array Position })
      "MultiLineString" ->
        map
          (MultiLineString <<< _.coordinates)
          (SimpleJSON.readImpl f :: F { coordinates :: Array (Array Position) })
      "MultiPolygon" ->
        map
          (MultiPolygon <<< _.coordinates)
          (SimpleJSON.readImpl f :: F { coordinates :: Array (Array (Array Position)) })
      "GeometryCollection" ->
        map
          (GeometryCollection <<< _.geometries)
          (SimpleJSON.readImpl f :: F { geometries :: Array GeometryObject })
      _ ->
        Foreign.fail (ForeignError "unknown Geometry Object type")

instance showGeometryObject :: Show GeometryObject where
  show (Point r) = "(Point " <> show r <> ")"
  show (LineString r) = "(LineString " <> show r <> ")"
  show (Polygon r) = "(Polygon " <> show r <> ")"
  show (MultiPoint r) = "(MultiPoint " <> show r <> ")"
  show (MultiLineString r) = "(MultiLineString " <> show r <> ")"
  show (MultiPolygon r) = "(MultiPolygon " <> show r <> ")"
  show (GeometryCollection r) = "(GeometryCollection " <> show r <> ")"

instance writeForeignGeometryObject :: WriteForeign GeometryObject where
  writeImpl (Point r) =
    SimpleJSON.writeImpl { type: "Point", coordinates: r }
  writeImpl (LineString r) =
    SimpleJSON.writeImpl { type: "LineString", coordinates: r }
  writeImpl (Polygon r) =
    SimpleJSON.writeImpl { type: "Polygon", coordinates: r }
  writeImpl (MultiPoint r) =
    SimpleJSON.writeImpl { type: "MultiPoint", coordinates: r }
  writeImpl (MultiLineString r) =
    SimpleJSON.writeImpl { type: "MultiLineString", coordinates: r }
  writeImpl (MultiPolygon r) =
    SimpleJSON.writeImpl { type: "MultiPolygon", coordinates: r }
  writeImpl (GeometryCollection r) =
    SimpleJSON.writeImpl { type: "GeometryCollection", geometries: r }

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
    o <- SimpleJSON.readImpl f :: F { type :: String }
    case o.type of
      "Feature" -> do
        { geometry, properties, id } <-
          SimpleJSON.readImpl f ::
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
                _ -> Foreign.fail (ForeignError "unknown Feature Object id type")
        pure
          (Feature
            (toMaybe geometry)
            (map SimpleJSON.writeJSON (toMaybe properties))
            id')
      _ ->
        Foreign.fail (ForeignError "unknown Feature Object type")

instance showFeatureObject :: Show FeatureObject where
  show (Feature g p i)
    = "(Feature"
    <> " geometry: " <> maybe "null" show g
    <> " properties: " <> maybe "null" identity p
    <> (maybe "" (\e -> " id: " <> (either show identity e)) i)
    <> ")"

instance writeForeignFeatureObject :: WriteForeign FeatureObject where
  writeImpl (Feature geometry properties id) =
    SimpleJSON.writeImpl
      { type: "Feature"
      , geometry: toNullable geometry
      , properties: toNullable (properties >>= SimpleJSON.readJSON_) :: Nullable Foreign
      , id: map (either SimpleJSON.writeImpl SimpleJSON.writeImpl) id
      }

data FeatureCollectionObject
  = FeatureCollection (Array FeatureObject)

derive instance eqFeatureCollectionObject :: Eq FeatureCollectionObject

instance readForeignFeatureCollectionObject :: ReadForeign FeatureCollectionObject where
  readImpl f = do
    o <- SimpleJSON.readImpl f :: F { type :: String }
    case o.type of
      "FeatureCollection" ->
        map
          (FeatureCollection <<< _.features)
          (SimpleJSON.readImpl f :: F { features :: Array FeatureObject })
      _ ->
        Foreign.fail (ForeignError "unknown FeatureCollection Object type")

instance showFeatureCollectionObject :: Show FeatureCollectionObject where
  show (FeatureCollection fs) = "(FeatureCollection " <> show fs <> ")"

instance writeForeignFeatureCollectionObject :: WriteForeign FeatureCollectionObject where
  writeImpl (FeatureCollection fs) =
    SimpleJSON.writeImpl
      { type: "FeatureCollection"
      , features: map SimpleJSON.writeImpl fs
      }

data GeoJSON
  = GeometryObject GeometryObject
  | FeatureObject FeatureObject
  | FeatureCollectionObject FeatureCollectionObject

derive instance eqGeoJSON :: Eq GeoJSON

instance readForeignGeoJSON :: ReadForeign GeoJSON where
  readImpl f = do
    o <- SimpleJSON.readImpl f :: F { type :: String }
    case o.type of
      "FeatureCollection" ->
        map FeatureCollectionObject (SimpleJSON.readImpl f :: F FeatureCollectionObject)
      "Feature" ->
        map FeatureObject (SimpleJSON.readImpl f :: F FeatureObject)
      "Point" ->
        map GeometryObject (SimpleJSON.readImpl f :: F GeometryObject)
      "LineString" ->
        map GeometryObject (SimpleJSON.readImpl f :: F GeometryObject)
      "Polygon" ->
        map GeometryObject (SimpleJSON.readImpl f :: F GeometryObject)
      "MultiPoint" ->
        map GeometryObject (SimpleJSON.readImpl f :: F GeometryObject)
      "MultiLineString" ->
        map GeometryObject (SimpleJSON.readImpl f :: F GeometryObject)
      "MultiPolygon" ->
        map GeometryObject (SimpleJSON.readImpl f :: F GeometryObject)
      "GeometryCollection" ->
        map GeometryObject (SimpleJSON.readImpl f :: F GeometryObject)
      _ ->
        Foreign.fail (ForeignError "unknown GeoJSON type")

instance showGeoJSON :: Show GeoJSON where
  show (GeometryObject o) = show o
  show (FeatureObject o) = show o
  show (FeatureCollectionObject o) = show o

instance writeForeignGeoJSON :: WriteForeign GeoJSON where
  writeImpl (GeometryObject o) = SimpleJSON.writeImpl o
  writeImpl (FeatureObject o) = SimpleJSON.writeImpl o
  writeImpl (FeatureCollectionObject o) = SimpleJSON.writeImpl o
