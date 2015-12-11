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

module Notebook.Cell.Search.Component
  ( searchComponent
  , module Notebook.Cell.Search.Component.Query
  , module Notebook.Cell.Search.Component.State
  ) where

import Prelude

import Control.Monad.Aff.Class as Aff

import Control.Monad.Trans as MT
import Control.Monad.Error.Class as EC
import Control.Monad.Writer.Class as WC

import Data.Either (either)
import Data.Foldable as F
import Data.Functor.Coproduct
import Data.Maybe as M
import Data.StrMap as SM

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed as HE
import Halogen.Themes.Bootstrap3 as B

import Render.CssClasses as CSS
import Render.Common as RC

import Model.CellType as CT
import Model.Notebook.Search as Search
import Model.Port as Port

import Notebook.Cell.Common.EvalQuery as NC
import Notebook.Cell.Component as NC
import Notebook.Cell.Search.Component.Query
import Notebook.Cell.Search.Component.State
import Notebook.FileInput.Component as FI
import Notebook.Common (Slam())

import Text.SlamSearch as SS

import Quasar.Aff as Quasar

type Query = Coproduct NC.CellEvalQuery SearchQuery

searchComponent :: Component NC.CellStateP NC.CellQueryP Slam
searchComponent =
  NC.makeEditorCellComponent
    { name: CT.cellName CT.Search
    , glyph: CT.cellGlyph CT.Search
    , component: parentComponent render eval
    , initialState: installedState initialSearchState
    , _State: NC._SearchState
    , _Query: NC.makeQueryPrism' NC._SearchQuery
    }

render :: SearchState -> ParentHTML FI.State Query FI.Query Slam Unit
render state =
  H.div
    [ P.class_ CSS.exploreCellEditor ]
    [ H.slot unit \_ -> { component: FI.fileInputComponent, initialState: FI.initialState }
    , H.div [ P.classes [ CSS.fileListField, B.inputGroup ] ]
        [ H.input
            [ P.classes [ B.formControl, CSS.searchCellInput ]
            , P.placeholder "Input search string"
            , HE.onValueInput $ HE.input \str -> UpdateSearch str >>> right
            , P.value state.searchString
            ]
        , H.img
            [ HE.onClick (HE.input_ $ UpdateSearch "" >>> right)
            , P.class_ CSS.searchClear
            , P.src $ if state.running then "img/spin.gif" else "img/remove.svg"
            ]
        , H.span [ P.class_ B.inputGroupBtn ]
            [ H.button
                [ P.classes [ B.btn, B.btnDefault, CSS.searchCellButton ]
                , P.buttonType P.ButtonButton
                , HE.onClick (HE.input_ $ NC.NotifyRunCell >>> left)
                ]
                [ RC.glyph B.glyphiconSearch
                ]
            ]
        ]
    ]

runWith :: forall s' f f' g p. Natural (ParentDSL SearchState s' f f' g p) (ParentDSL SearchState s' f f' g p)
runWith m = do
  modify (_ { running = true })
  x <- m
  modify (_ { running = false })
  pure x

eval :: Natural Query (ParentDSL SearchState FI.State Query FI.Query Slam Unit)
eval = coproduct cellEval searchEval
  where
    cellEval :: Natural NC.CellEvalQuery (ParentDSL SearchState FI.State Query FI.Query Slam Unit)
    cellEval q =
      case q of
        NC.EvalCell info k -> runWith do
          k <$> NC.runCellEvalT do
            inputResource <-
              query unit (request FI.GetSelectedFile) <#> (>>= id)
                # MT.lift
                >>= M.maybe (EC.throwError "No file selected") pure
            query <-
              get <#> _.searchString >>> SS.mkQuery
                # MT.lift
                >>= either (\_ -> EC.throwError "Incorrect query string") pure

            fields <- MT.lift <<< liftH <<< liftAff' $ Quasar.fields inputResource

            let
              template = Search.queryToSQL fields query
              sql = Quasar.templated inputResource template
              tempOutputResource = NC.temporaryOutputResource info

            WC.tell ["Generated SQL: " <> sql]

            { plan: plan, outputResource: outputResource } <-
              Quasar.executeQuery template (M.fromMaybe false info.cachingEnabled) SM.empty inputResource tempOutputResource
                # Aff.liftAff >>> liftH >>> liftH >>> MT.lift
                >>= either (\err -> EC.throwError $ "Error in query: " <> err) pure

            F.for_ plan \p ->
              WC.tell ["Plan: " <> p]

            pure $ Port.Resource outputResource

        NC.NotifyRunCell next ->
          pure next

    searchEval :: Natural SearchQuery (ParentDSL SearchState FI.State Query FI.Query Slam Unit)
    searchEval q =
      case q of
        UpdateSearch str next -> do
          modify (_ { searchString = str })
          pure next
