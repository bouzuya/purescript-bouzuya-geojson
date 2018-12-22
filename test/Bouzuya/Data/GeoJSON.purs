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
      Assert.equal
        (Just
          (Point [100.0, 0.0]))
        (SimpleJSON.readJSON_ """
{
  "type": "Point",
  "coordinates": [100.0, 0.0]
}
        """ :: Maybe GeometryObject)
    test "LineString" do
      Assert.equal
        (Just
          (LineString
            [ [100.0, 0.0]
            , [101.0, 1.0]
            ]))
        (SimpleJSON.readJSON_ """
{
  "type": "LineString",
  "coordinates": [
    [100.0, 0.0],
    [101.0, 1.0]
  ]
}
        """ :: Maybe GeometryObject)
    test "Polygon" do
      Assert.equal
        (Just
          (Polygon
            [ [ [100.0, 0.0]
              , [101.0, 0.0]
              , [101.0, 1.0]
              , [100.0, 1.0]
              , [100.0, 0.0]
              ]
            ]))
        (SimpleJSON.readJSON_ """
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
        """ :: Maybe GeometryObject)

      Assert.equal
        (Just
          (Polygon
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
            ]))
        (SimpleJSON.readJSON_ """
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
        """ :: Maybe GeometryObject)
    test "MultiPoint" do
      Assert.equal
        (Just
          (MultiPoint
            [ [100.0, 0.0]
            , [101.0, 1.0]
            ]))
        (SimpleJSON.readJSON_ """
{
  "type": "MultiPoint",
  "coordinates": [
    [100.0, 0.0],
    [101.0, 1.0]
  ]
}
        """ :: Maybe GeometryObject)
    test "MultiLineString" do
      Assert.equal
        (Just
          (MultiLineString
            [ [ [100.0, 0.0]
              , [101.0, 1.0]
              ]
            , [ [102.0, 2.0]
              , [103.0, 3.0]
              ]
            ]))
        (SimpleJSON.readJSON_ """
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
        """ :: Maybe GeometryObject)

    test "MultiPolygon" do
      Assert.equal
        (Just
          (MultiPolygon
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
            ]))
        (SimpleJSON.readJSON_ """
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
        """ :: Maybe GeometryObject)

    test "GeometryCollection" do
      Assert.equal
        (Just
          (GeometryCollection
            [ Point [100.0, 0.0]
            , LineString
              [ [101.0, 0.0]
              , [102.0, 1.0]
              ]
            ]))
        (SimpleJSON.readJSON_ """
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
        """ :: Maybe GeometryObject)

    test "Feature" do
      Assert.equal
        (Just
          (Feature
            (Just (Point [102.0, 0.5]))
            (Just (SimpleJSON.writeJSON { prop0: "value0" }))
            Nothing
            ))
        (SimpleJSON.readJSON_ """
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
        """ :: Maybe FeatureObject)
      Assert.equal
        (Just
          (Feature
            Nothing
            Nothing
            (Just (Right "f1"))
            ))
        (SimpleJSON.readJSON_ """
{
  "type": "Feature",
  "id": "f1",
  "geometry": null,
  "properties": null,
  "title": "Example Feature"
}
        """ :: Maybe FeatureObject)

    test "FeatureCollection" do
      Assert.equal
        (Just
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
            ]))
        (SimpleJSON.readJSON_ """
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
        """ :: Maybe FeatureCollectionObject)

    test "GeoJSON" do
      Assert.equal
        (Just
          (GeoJSON.FeatureCollectionObject
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
              ])))
        (SimpleJSON.readJSON_ """
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
        """ :: Maybe GeoJSON)
