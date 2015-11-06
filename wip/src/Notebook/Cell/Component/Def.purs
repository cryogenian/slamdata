module Notebook.Cell.Component.Def
  ( EditorDef()
  , ResultsDef()
  , CellProps()
  ) where

import Prelude

import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)
import Data.Lens (APrismP())

import Halogen (Component())
import Halogen.HTML.Indexed as H

import Notebook.Cell.Common.EvalQuery (CellEvalQuery())
import Notebook.Cell.Component.Query (AnyCellQuery())
import Notebook.Cell.Component.State (AnyCellState())
import Notebook.Common (Slam())

type CellProps s f r =
  ( component :: Component s (Coproduct CellEvalQuery f) Slam
  , initialState :: s
  , _State :: APrismP AnyCellState s
  , _Query :: forall a. APrismP (AnyCellQuery a) (f a)
  | r
  )

type EditorDef s f =
  { name :: String
  , glyph :: H.ClassName
  | CellProps s f ()
  }

type ResultsDef s f = Object (CellProps s f ())
