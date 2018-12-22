module Test.Main
  ( main
  ) where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Main (GeometryObject(..))
import Prelude (Unit, discard)
import Simple.JSON as SimpleJSON
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
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
