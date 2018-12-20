module Test.Main
  ( main
  ) where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Main (GeoJSON(..), parse)
import Prelude (Unit, discard)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  suite "Geometry Object" do
    test "Point" do
      Assert.equal
        (Just
          (Point
            { type: "Point"
            , coordinates: [100.0, 0.0]
            }))
        (parse """
{
  "type": "Point",
  "coordinates": [100.0, 0.0]
}
        """)
    test "LineString" do
      Assert.equal
        (Just
          (LineString
            { type: "LineString"
            , coordinates:
              [ [100.0, 0.0]
              , [101.0, 1.0]
              ]
            }))
        (parse """
{
  "type": "LineString",
  "coordinates": [ [100.0, 0.0], [101.0, 1.0] ]
}
        """)
