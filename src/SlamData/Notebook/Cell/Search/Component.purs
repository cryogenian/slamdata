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

module SlamData.Notebook.Cell.Search.Component
  ( searchComponent
  , module SlamData.Notebook.Cell.Search.Component.Query
  , module SlamData.Notebook.Cell.Search.Component.State
  ) where

import Prelude

import Control.Bind (join)
import Control.Monad (when)
import Control.Monad.Error.Class as EC
import Control.Monad.Trans as MT
import Control.Monad.Writer.Class as WC

import Data.Either (Either(..), either)
import Data.Foldable as F
import Data.Functor (($>))
import Data.Functor.Coproduct
import Data.Lens ((.~), preview)
import Data.Maybe as M
import Data.StrMap as SM

import Halogen
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import Quasar.Aff as Quasar
import Quasar.Auth as Auth

import SlamData.FileSystem.Resource as R
import SlamData.Notebook.Cell.CellType as CT
import SlamData.Notebook.Cell.Common.EvalQuery as NC
import SlamData.Notebook.Cell.Component as NC
import SlamData.Notebook.Cell.Port as Port
import SlamData.Notebook.Cell.Search.Component.Query
import SlamData.Notebook.Cell.Search.Component.State
import SlamData.Notebook.Cell.Search.Interpret as Search
import SlamData.Notebook.Cell.Search.Model as Model
import SlamData.Effects (Slam())
import SlamData.Notebook.FileInput.Component as FI
import SlamData.Render.Common as RC
import SlamData.Render.CSS as CSS

import Text.SlamSearch as SS

searchComponent :: Component NC.CellStateP NC.CellQueryP Slam
searchComponent =
  NC.makeEditorCellComponent
    { name: CT.cellName CT.Search
    , glyph: CT.cellGlyph CT.Search
    , component: parentComponent render eval
    , initialState: installedState initialState
    , _State: NC._SearchState
    , _Query: NC.makeQueryPrism' NC._SearchQuery
    }

render :: State -> ParentHTML FI.State Query FI.Query Slam Unit
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

runWith :: forall s' f f' g p. Natural (ParentDSL State s' f f' g p) (ParentDSL State s' f f' g p)
runWith m = do
  modify (_running .~ true)
  x <- m
  modify (_running .~ false)
  pure x

eval :: Natural Query (ParentDSL State FI.State Query FI.Query Slam Unit)
eval = coproduct cellEval searchEval

cellEval :: Natural NC.CellEvalQuery (ParentDSL State FI.State Query FI.Query Slam Unit)
cellEval q =
  case q of
    NC.EvalCell info k -> runWith do
      k <$> NC.runCellEvalT do
        inputResource <-
          query unit (request FI.GetSelectedFile)
          <#> (join <<< M.maybe (Left "There is no file input subcomponent") Right)
          # MT.lift
          >>= either EC.throwError pure
        query <-
          get <#> _.searchString >>> SS.mkQuery
            # MT.lift
            >>= either (\_ -> EC.throwError "Incorrect query string") pure

        (MT.lift $ NC.liftWithCanceler' $ Auth.authed $ Quasar.resourceExists inputResource)
          >>= \x -> when (not x) $ EC.throwError $ "Input resource "
            <> R.resourcePath inputResource
            <> " doesn't exist"
        fields <- MT.lift <<< NC.liftWithCanceler' $ Auth.authed $ Quasar.fields inputResource

        let
          template = Search.queryToSQL fields query
          sql = Quasar.templated inputResource template
          tempOutputResource = NC.temporaryOutputResource info

        WC.tell ["Generated SQL: " <> sql]

        { plan: plan, outputResource: outputResource } <-
          Quasar.executeQuery template (M.fromMaybe false info.cachingEnabled) SM.empty inputResource tempOutputResource
            # Auth.authed
            # NC.liftWithCanceler' >>> MT.lift
            >>= either (\err -> EC.throwError $ "Error in query: " <> err) pure

        F.for_ plan \p ->
          WC.tell ["Plan: " <> p]

        (MT.lift $ NC.liftWithCanceler' $ Auth.authed $ Quasar.resourceExists outputResource)
          >>= \x -> when (not x)
                    $ EC.throwError "Error making search temporary resource"

        pure $ Port.TaggedResource { resource: outputResource, tag: pure sql }

    NC.SetupCell { inputPort } next -> do
      case preview Port._Resource inputPort of
        M.Just res -> query unit (action (FI.SelectFile res)) $> next
        M.Nothing -> pure next

    NC.NotifyRunCell next ->
      pure next

    NC.Save k -> do
      file <- query unit (request FI.GetSelectedFile)
      input <- gets _.searchString
      pure $ k $ Model.encode { input, file: case file of
                                  M.Just (Right res) -> M.Just res
                                  _ -> M.Nothing
                              }
    NC.Load json next -> do
      case Model.decode json of
        Left _ -> pure unit
        Right { file, input } -> do
          M.maybe (pure unit) (void <<< query unit <<< action <<< FI.SelectFile) file
          modify (_searchString .~ input)
      pure next
    NC.SetCanceler _ next -> pure next

searchEval :: Natural SearchQuery (ParentDSL State FI.State Query FI.Query Slam Unit)
searchEval q =
  case q of
    UpdateSearch str next -> do
      modify (_searchString .~ str)
      pure next
