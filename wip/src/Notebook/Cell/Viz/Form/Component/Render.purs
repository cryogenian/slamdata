module Notebook.Cell.Viz.Form.Component.Render where

import Data.Maybe (Maybe(..))
import Halogen.HTML.Indexed as H
import Halogen.Themes.Bootstrap3 as B

type GridClasses =
  { first :: Array H.ClassName
  , second :: Array H.ClassName
  , third :: Array H.ClassName
  }

gridClasses :: forall a. Maybe a -> Maybe a -> Maybe a -> GridClasses
gridClasses Nothing Nothing Nothing =
  { first: [ ]
  , second: [ ]
  , third: [ ]
  }
gridClasses (Just _) Nothing Nothing =
  { first: [ B.colXs4 ]
  , second: [ ]
  , third: [ ]
  }
gridClasses Nothing (Just _) Nothing =
  { first: [ ]
  , second: [ B.colXs4, B.colXsOffset4 ]
  , third: [ ]
  }
gridClasses Nothing Nothing (Just _) =
  { first: [ ]
  , second: [ ]
  , third: [ B.colXs4, B.colXsOffset8 ]
  }
gridClasses Nothing (Just _) (Just _) =
  { first: [ ]
  , second: [ B.colXs4, B.colXsOffset4 ]
  , third: [ B.colXs4 ]
  }
gridClasses (Just _) Nothing (Just _) =
  { first: [ B.colXs4 ]
  , second: [ ]
  , third: [ B.colXs4, B.colXsOffset4 ]
  }
gridClasses (Just _) (Just _) Nothing =
  { first: [ B.colXs4 ]
  , second: [ B.colXs4 ]
  , third: [ ]
  }
gridClasses _ _ _ =
  { first: [ B.colXs4 ]
  , second: [ B.colXs4 ]
  , third: [ B.colXs4 ]
  }
