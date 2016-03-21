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

module SlamData.Notebook.Cell.Explore.Component
  ( exploreComponent
  , module SlamData.Notebook.Cell.Explore.Component.Query
  , module SlamData.Notebook.Cell.Explore.Component.State
  ) where

import SlamData.Prelude

import Control.Monad.Error.Class as EC

import Data.Argonaut (encodeJson, decodeJson)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import Quasar.Aff as Quasar
import Quasar.Auth as Auth

import SlamData.FileSystem.Resource as R
import SlamData.Notebook.Cell.CellType as CT
import SlamData.Notebook.Cell.Common.EvalQuery (runCellEvalT, liftWithCanceler)
import SlamData.Notebook.Cell.Component as NC
import SlamData.Notebook.Cell.Explore.Component.Query (Query(..), QueryP)
import SlamData.Notebook.Cell.Explore.Component.State (State, StateP, initialState)
import SlamData.Notebook.Cell.Port as Port
import SlamData.Effects (Slam)
import SlamData.Notebook.FileInput.Component as FI
import SlamData.Render.CSS as CSS

exploreComponent :: H.Component NC.CellStateP NC.CellQueryP Slam
exploreComponent =
  NC.makeEditorCellComponent
    { name: CT.cellName CT.Explore
    , glyph: CT.cellGlyph CT.Explore
    , component: H.parentComponent { render, eval, peek: Nothing }
    , initialState: H.parentState initialState
    , _State: NC._ExploreState
    , _Query: NC.makeQueryPrism NC._ExploreQuery
    }

render :: State -> H.ParentHTML FI.State NC.CellEvalQuery FI.Query Slam Unit
render state =
  HH.div
    [ HP.classes [ CSS.exploreCellEditor, CSS.cellInput ] ]
    [ HH.slot unit \_ ->
        { component: FI.fileInputComponent
        , initialState: FI.initialState
        }
    ]

eval
  :: Natural
       NC.CellEvalQuery
       (H.ParentDSL State FI.State NC.CellEvalQuery FI.Query Slam Unit)
eval (NC.NotifyRunCell next) = pure next
eval (NC.EvalCell info k) =
  k <$> runCellEvalT do
    resource <-
      H.query unit (H.request FI.GetSelectedFile)
        <#> (join <<< maybe (Left "There is no file input subcomponent") Right)
        # lift
        >>= either EC.throwError pure
    Quasar.messageIfResourceNotExists
        resource
        ("File " <> R.resourcePath resource <> " doesn't exist")
      # Auth.authed
      # liftWithCanceler
      # lift
      >>= traverse_ EC.throwError
    pure $ Port.TaggedResource {resource, tag: Nothing}
eval (NC.SetupCell _ next) = pure next
eval (NC.Save k) = do
  file <- H.query unit (H.request FI.GetSelectedFile)
  pure $ k $ encodeJson $ either (const Nothing) pure =<< file
eval (NC.Load json next) = do
  for_ (decodeJson json) \file ->
    void $ H.query unit $ H.action (FI.SelectFile file)
  pure next
eval (NC.SetCanceler _ next) = pure next
