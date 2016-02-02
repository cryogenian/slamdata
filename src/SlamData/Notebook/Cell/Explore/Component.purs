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

module SlamData.Notebook.Cell.Explore.Component
  ( exploreComponent
  , module SlamData.Notebook.Cell.Explore.Component.Query
  , module SlamData.Notebook.Cell.Explore.Component.State
  ) where

import Prelude

import Control.Bind (join)
import Control.Monad (when)
import Control.Monad.Error.Class as EC
import Control.Monad.Trans as MT

import Data.Argonaut (encodeJson, decodeJson)
import Data.Either (either)
import Data.Functor.Aff (liftAff)
import Data.Maybe (Maybe(..), maybe)

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Quasar.Aff as Quasar

import SlamData.FileSystem.Resource as R
import SlamData.Notebook.Cell.CellType as CT
import SlamData.Notebook.Cell.Common.EvalQuery as NC
import SlamData.Notebook.Cell.Component as NC
import SlamData.Notebook.Cell.Explore.Component.Query
import SlamData.Notebook.Cell.Explore.Component.State
import SlamData.Notebook.Cell.Port as Port
import SlamData.Notebook.Effects (Slam())
import SlamData.Notebook.FileInput.Component as FI
import SlamData.Render.CSS as CSS

exploreComponent :: Component NC.CellStateP NC.CellQueryP Slam
exploreComponent =
  NC.makeEditorCellComponent
    { name: CT.cellName CT.Explore
    , glyph: CT.cellGlyph CT.Explore
    , component: parentComponent render eval
    , initialState: installedState initialState
    , _State: NC._ExploreState
    , _Query: NC.makeQueryPrism NC._ExploreQuery
    }

render :: State -> ParentHTML FI.State NC.CellEvalQuery FI.Query Slam Unit
render state =
  H.div
    [ P.class_ CSS.exploreCellEditor ]
    [ H.slot unit \_ -> { component: FI.fileInputComponent, initialState: FI.initialState } ]

eval :: Natural NC.CellEvalQuery (ParentDSL State FI.State NC.CellEvalQuery FI.Query Slam Unit)
eval (NC.NotifyRunCell next) = pure next
eval (NC.EvalCell info k) =
  k <$> NC.runCellEvalT do
    resource <-
      query unit (request FI.GetSelectedFile) <#> (>>= id)
        # MT.lift
        >>= maybe (EC.throwError "No file selected") pure
    (MT.lift $ liftAff $ Quasar.resourceExists resource)
      >>= \x -> when (not x) $ EC.throwError
                $ "File " <> R.resourcePath resource <> " doesn't exist"

    pure $ Port.Resource resource
eval (NC.SetupCell _ next) = pure next
eval (NC.Save k) = do
  file <- query unit (request FI.GetSelectedFile)
  pure $ k $ encodeJson (join file)
eval (NC.Load json next) = do
  let file = either (const Nothing) id $ decodeJson json
  maybe (pure unit) (\file' -> void $ query unit $ action (FI.SelectFile file')) file
  pure next
