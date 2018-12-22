module Test.Main
  ( main
  ) where

import Effect (Effect)
import Prelude (Unit)
import Test.Bouzuya.Data.GeoJSON as GeoJSON
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  GeoJSON.tests
