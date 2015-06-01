module Controller.Notebook.Cell.Viz.Key where

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


