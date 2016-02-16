{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Notebook.Cell.Component.Def
  ( EditorCellDef()
  , ResultsCellDef()
  , CellDefProps()
  , makeQueryPrism
  , makeQueryPrism'
  ) where

import Prelude

import Data.Functor.Coproduct (Coproduct(), coproduct, left, right)
import Data.Lens (APrismP(), PrismP(), prism', review, preview)
import Data.Maybe (Maybe(..))

import Halogen (Component())
import Halogen.HTML.Indexed as H

import SlamData.Notebook.Cell.Common.EvalQuery (CellEvalQuery())
import SlamData.Notebook.Cell.Component.Query (AnyCellQuery())
import SlamData.Notebook.Cell.Component.State (AnyCellState())
import SlamData.Effects (Slam())

-- | The type for the definition of an editor cell component.
type EditorCellDef s f =
  { name :: String
  , glyph :: H.ClassName
  | CellDefProps s f ()
  }

-- | The type for the definition of an results component.
type ResultsCellDef s f = Object (CellDefProps s f ())

-- | The properties required by both types of cell definition.
type CellDefProps s f r =
  ( component :: Component s f Slam
  , initialState :: s
  , _State :: APrismP AnyCellState s
  , _Query :: forall a. APrismP (Coproduct CellEvalQuery AnyCellQuery a) (f a)
  | r
  )

-- | Makes a prism for `_Query` for cell components with a query algebra of the
-- | form `Coproduct CellEvalQuery f`.
-- |
-- | This applies to two types of cell components:
-- |
-- | 1. Parent components where the parent uses `CellEvalQuery` directly, in
-- |    which case `f` will be some `ChildF`.
-- | 2. Self contained components with an enriched query algebra, where `f`
-- |    will be the component's own internal algebra.
makeQueryPrism
  :: forall a f
   . PrismP (AnyCellQuery a) (Coproduct CellEvalQuery f a)
  -> PrismP (Coproduct CellEvalQuery AnyCellQuery a) (Coproduct CellEvalQuery f a)
makeQueryPrism base = prism' to fro
  where
  to :: Coproduct CellEvalQuery f a -> Coproduct CellEvalQuery AnyCellQuery a
  to = coproduct left (right <<< review base <<< right)
  fro :: Coproduct CellEvalQuery AnyCellQuery a -> Maybe (Coproduct CellEvalQuery f a)
  fro = coproduct (Just <<< left) (preview base)

-- | Makes a prism for `_Query` for cell components with a query algebra of the
-- | form `Coproduct (Coproduct CellEvalQuery f) g`.
-- |
-- | This will occurs when a cell component is a parent component and also has
-- | an enriched query algebra, where `f` is the component's internal query
-- | algebra and `g` will be some `ChildF`.
makeQueryPrism'
  :: forall a f g
   . PrismP (AnyCellQuery a) (Coproduct (Coproduct CellEvalQuery f) g a)
  -> PrismP (Coproduct CellEvalQuery AnyCellQuery a) (Coproduct (Coproduct CellEvalQuery f) g a)
makeQueryPrism' base = prism' to fro
  where
  to :: Coproduct (Coproduct CellEvalQuery f) g a -> Coproduct CellEvalQuery AnyCellQuery a
  to = coproduct (coproduct left (right <<< review base <<< left <<< right)) (right <<< review base <<< right)
  fro :: Coproduct CellEvalQuery AnyCellQuery a -> Maybe (Coproduct (Coproduct CellEvalQuery f) g a)
  fro = coproduct (Just <<< left <<< left) (preview base)
