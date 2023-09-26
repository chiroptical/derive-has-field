module Import where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)

dropPrefix :: String -> String -> String
dropPrefix prefix input = fromMaybe input $ stripPrefix prefix input
