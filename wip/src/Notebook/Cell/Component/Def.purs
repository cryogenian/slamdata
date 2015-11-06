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
