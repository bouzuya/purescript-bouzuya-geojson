module Test.Bouzuya.Data.GeoJSON
  ( tests
  ) where

import Bouzuya.Data.GeoJSON (FeatureCollectionObject(..), FeatureObject(..), GeoJSON, GeometryObject(..))
import Bouzuya.Data.GeoJSON as GeoJSON
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import Prelude (discard, map)
import Simple.JSON as SimpleJSON
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests =
  suite "Geometry Object" do
    test "Point" do
      let
        o = Point [100.0, 0.0]
        t' = """
{
  "type": "Point",
  "coordinates": [100.0, 0.0]
}
        """
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ t' :: Maybe GeometryObject)
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ (SimpleJSON.writeJSON o))

    test "LineString" do
      let
        o =
          LineString
            [ [100.0, 0.0]
            , [101.0, 1.0]
            ]
        t' = """
{
  "type": "LineString",
  "coordinates": [
    [100.0, 0.0],
    [101.0, 1.0]
  ]
}
        """
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ t' :: Maybe GeometryObject)
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ (SimpleJSON.writeJSON o))

    test "Polygon" do
      let
        o =
          Polygon
            [ [ [100.0, 0.0]
              , [101.0, 0.0]
              , [101.0, 1.0]
              , [100.0, 1.0]
              , [100.0, 0.0]
              ]
            ]
        t' = """
{
  "type": "Polygon",
  "coordinates": [
    [
      [100.0, 0.0],
      [101.0, 0.0],
      [101.0, 1.0],
      [100.0, 1.0],
      [100.0, 0.0]
    ]
  ]
}
        """
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ t' :: Maybe GeometryObject)
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ (SimpleJSON.writeJSON o))
      let
        o2 =
          Polygon
            [ [ [100.0, 0.0]
              , [101.0, 0.0]
              , [101.0, 1.0]
              , [100.0, 1.0]
              , [100.0, 0.0]
              ],
              [ [100.8, 0.8]
              , [100.8, 0.2]
              , [100.2, 0.2]
              , [100.2, 0.8]
              , [100.8, 0.8]
              ]
            ]
        t'2 = """
{
  "type": "Polygon",
  "coordinates": [
    [
      [100.0, 0.0],
      [101.0, 0.0],
      [101.0, 1.0],
      [100.0, 1.0],
      [100.0, 0.0]
    ],
    [
      [100.8, 0.8],
      [100.8, 0.2],
      [100.2, 0.2],
      [100.2, 0.8],
      [100.8, 0.8]
    ]
  ]
}
        """
      Assert.equal
        (Just o2)
        (SimpleJSON.readJSON_ t'2 :: Maybe GeometryObject)
      Assert.equal
        (Just o2)
        (SimpleJSON.readJSON_ (SimpleJSON.writeJSON o2))

    test "MultiPoint" do
      let
        o =
          MultiPoint
            [ [100.0, 0.0]
            , [101.0, 1.0]
            ]
        t' = """
{
  "type": "MultiPoint",
  "coordinates": [
    [100.0, 0.0],
    [101.0, 1.0]
  ]
}
        """
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ t' :: Maybe GeometryObject)
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ (SimpleJSON.writeJSON o))

    test "MultiLineString" do
      let
        o =
          MultiLineString
            [ [ [100.0, 0.0]
              , [101.0, 1.0]
              ]
            , [ [102.0, 2.0]
              , [103.0, 3.0]
              ]
            ]
        t' = """
{
  "type": "MultiLineString",
  "coordinates": [
    [
      [100.0, 0.0],
      [101.0, 1.0]
    ],
    [
      [102.0, 2.0],
      [103.0, 3.0]
    ]
  ]
}
        """
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ t' :: Maybe GeometryObject)
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ (SimpleJSON.writeJSON o))

    test "MultiPolygon" do
      let
        o =
          MultiPolygon
            [ [ [ [102.0, 2.0]
                , [103.0, 2.0]
                , [103.0, 3.0]
                , [102.0, 3.0]
                , [102.0, 2.0]
                ]
              ]
            , [ [ [100.0, 0.0]
                , [101.0, 0.0]
                , [101.0, 1.0]
                , [100.0, 1.0]
                , [100.0, 0.0]
                ]
              , [ [100.2, 0.2]
                , [100.2, 0.8]
                , [100.8, 0.8]
                , [100.8, 0.2]
                , [100.2, 0.2]
                ]
              ]
            ]
        t' = """
{
  "type": "MultiPolygon",
  "coordinates": [
    [
      [
        [102.0, 2.0],
        [103.0, 2.0],
        [103.0, 3.0],
        [102.0, 3.0],
        [102.0, 2.0]
      ]
    ],
    [
      [
        [100.0, 0.0],
        [101.0, 0.0],
        [101.0, 1.0],
        [100.0, 1.0],
        [100.0, 0.0]
      ],
      [
        [100.2, 0.2],
        [100.2, 0.8],
        [100.8, 0.8],
        [100.8, 0.2],
        [100.2, 0.2]
      ]
    ]
  ]
}
        """
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ t' :: Maybe GeometryObject)
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ (SimpleJSON.writeJSON o))

    test "GeometryCollection" do
      let
        o =
          GeometryCollection
            [ Point [100.0, 0.0]
            , LineString
              [ [101.0, 0.0]
              , [102.0, 1.0]
              ]
            ]
        t' = """
{
  "type": "GeometryCollection",
  "geometries": [{
    "type": "Point",
    "coordinates": [100.0, 0.0]
  }, {
    "type": "LineString",
    "coordinates": [
      [101.0, 0.0],
      [102.0, 1.0]
    ]
  }]
}
        """
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ t' :: Maybe GeometryObject)
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ (SimpleJSON.writeJSON o))

    test "Feature" do
      let
        o =
          Feature
            (Just (Point [102.0, 0.5]))
            (Just (SimpleJSON.writeJSON { prop0: "value0" }))
            Nothing
        t' = """
{
  "type": "Feature",
  "geometry": {
    "type": "Point",
    "coordinates": [102.0, 0.5]
  },
  "properties": {
    "prop0": "value0"
  }
}
        """
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ t' :: Maybe FeatureObject)
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ (SimpleJSON.writeJSON o))

      let
        o2 =
          Feature
            Nothing
            Nothing
            (Just (Right "f1"))
        t'2 = """
{
  "type": "Feature",
  "id": "f1",
  "geometry": null,
  "properties": null,
  "title": "Example Feature"
}
        """
      Assert.equal
        (Just o2)
        (SimpleJSON.readJSON_ t'2 :: Maybe FeatureObject)
      Assert.equal
        (Just o2)
        (SimpleJSON.readJSON_ (SimpleJSON.writeJSON o2))

    test "FeatureCollection" do
      let
        o =
          FeatureCollection
            [ (Feature
                (Just (Point [102.0, 0.5]))
                (Just (SimpleJSON.writeJSON { prop0: "value0" }))
                Nothing)
            , (Feature
                (Just
                  (LineString
                    [ [102.0, 0.0]
                    , [103.0, 1.0]
                    , [104.0, 0.0]
                    , [105.0, 1.0]
                    ]))
                (map SimpleJSON.writeJSON (SimpleJSON.readJSON_ """
{
  "prop0": "value0",
  "prop1": 0.0
}
                """ :: Maybe Foreign))
                Nothing)
            , (Feature
                (Just
                  (Polygon
                    [ [ [100.0, 0.0]
                      , [101.0, 0.0]
                      , [101.0, 1.0]
                      , [100.0, 1.0]
                      , [100.0, 0.0]
                      ]
                    ]))
                (map SimpleJSON.writeJSON (SimpleJSON.readJSON_ """
{
  "prop0": "value0",
  "prop1": {
    "this": "that"
  }
}
                """ :: Maybe Foreign))
                Nothing)
            ]
        t' = """
{
  "type": "FeatureCollection",
  "features": [{
    "type": "Feature",
    "geometry": {
      "type": "Point",
      "coordinates": [102.0, 0.5]
    },
    "properties": {
      "prop0": "value0"
    }
  }, {
    "type": "Feature",
    "geometry": {
      "type": "LineString",
      "coordinates": [
        [102.0, 0.0],
        [103.0, 1.0],
        [104.0, 0.0],
        [105.0, 1.0]
      ]
    },
    "properties": {
      "prop0": "value0",
      "prop1": 0.0
    }
  }, {
    "type": "Feature",
    "geometry": {
      "type": "Polygon",
      "coordinates": [
        [
          [100.0, 0.0],
          [101.0, 0.0],
          [101.0, 1.0],
          [100.0, 1.0],
          [100.0, 0.0]
        ]
      ]
    },
    "properties": {
      "prop0": "value0",
      "prop1": {
        "this": "that"
      }
    }
  }]
}
        """
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ t' :: Maybe FeatureCollectionObject)
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ (SimpleJSON.writeJSON o))

    test "GeoJSON" do
      let
        o =
          GeoJSON.FeatureCollectionObject
            (FeatureCollection
              [ (Feature
                  (Just (Point [102.0, 0.5]))
                  (Just (SimpleJSON.writeJSON { prop0: "value0" }))
                  Nothing)
              , (Feature
                  (Just
                    (LineString
                      [ [102.0, 0.0]
                      , [103.0, 1.0]
                      , [104.0, 0.0]
                      , [105.0, 1.0]
                      ]))
                  (map SimpleJSON.writeJSON (SimpleJSON.readJSON_ """
  {
    "prop0": "value0",
    "prop1": 0.0
  }
                  """ :: Maybe Foreign))
                  Nothing)
              , (Feature
                  (Just
                    (Polygon
                      [ [ [100.0, 0.0]
                        , [101.0, 0.0]
                        , [101.0, 1.0]
                        , [100.0, 1.0]
                        , [100.0, 0.0]
                        ]
                      ]))
                  (map SimpleJSON.writeJSON (SimpleJSON.readJSON_ """
  {
    "prop0": "value0",
    "prop1": {
      "this": "that"
    }
  }
                  """ :: Maybe Foreign))
                  Nothing)
              ])
        t' = """
{
  "type": "FeatureCollection",
  "features": [{
    "type": "Feature",
    "geometry": {
      "type": "Point",
      "coordinates": [102.0, 0.5]
    },
    "properties": {
      "prop0": "value0"
    }
  }, {
    "type": "Feature",
    "geometry": {
      "type": "LineString",
      "coordinates": [
        [102.0, 0.0],
        [103.0, 1.0],
        [104.0, 0.0],
        [105.0, 1.0]
      ]
    },
    "properties": {
      "prop0": "value0",
      "prop1": 0.0
    }
  }, {
    "type": "Feature",
    "geometry": {
      "type": "Polygon",
      "coordinates": [
        [
          [100.0, 0.0],
          [101.0, 0.0],
          [101.0, 1.0],
          [100.0, 1.0],
          [100.0, 0.0]
        ]
      ]
    },
    "properties": {
      "prop0": "value0",
      "prop1": {
        "this": "that"
      }
    }
  }]
}
        """
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ t' :: Maybe GeoJSON)
      Assert.equal
        (Just o)
        (SimpleJSON.readJSON_ (SimpleJSON.writeJSON o))
