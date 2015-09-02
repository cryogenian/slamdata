{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Controller.Notebook.Cell.Viz.Key where

import Prelude
import Data.Tuple
import Data.Maybe

-- Tuple category $ Just (Tuple firstSeries (Just secondSeries))
type Key = Tuple String SeriesKey
type SeriesKey = Maybe (Tuple String (Maybe String))

keyCategory :: Key -> String
keyCategory (Tuple cat _) = cat

keyMbSeries1 :: Key -> Maybe String
keyMbSeries1 (Tuple _ mbT) = mbT >>= (pure <<< fst)

keyMbSeries2 :: Key -> Maybe String
keyMbSeries2 (Tuple _ mbT) = mbT >>= snd

mkKey :: String -> Maybe String -> Maybe String -> Key
mkKey cat f s =
  Tuple cat (f >>= \f -> pure $ Tuple f s)

keyName :: Key -> String
keyName k =
  (fromMaybe "" (keyMbSeries1 k)) <> (maybe "" (":" <>) (keyMbSeries2 k))


