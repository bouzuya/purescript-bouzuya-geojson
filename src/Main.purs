module Main
  ( parse
  ) where

import Data.Maybe (Maybe)
import Simple.JSON as SimpleJSON

type GeoJSON =
  { type :: String
  , coordinates :: Array Number
  }

parse :: String -> Maybe GeoJSON
parse = SimpleJSON.readJSON_
