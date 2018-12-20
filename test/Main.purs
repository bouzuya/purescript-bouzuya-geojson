module Test.Main
  ( main
  ) where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Main (parse)
import Prelude (Unit, discard)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  suite "Geometry Object" do
    test "Point" do
      Assert.equal
        (Just { type: "Point", coordinates: [100.0, 0.0] })
        (parse """
{
  "type": "Point",
  "coordinates": [100.0, 0.0]
}""")
