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

module SlamData.Notebook.Cell.Markdown.Component
  ( markdownComponent
  , queryShouldRun
  , module SlamData.Notebook.Cell.Markdown.Component.Query
  , module SlamData.Notebook.Cell.Markdown.Component.State
  ) where

import SlamData.Prelude

import Data.BrowserFeatures (BrowserFeatures)
import Data.StrMap as SM

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Notebook.Cell.CellId (CellId, runCellId)
import SlamData.Notebook.Cell.Common.EvalQuery (CellEvalQuery(..), CellEvalResult)
import SlamData.Notebook.Cell.Component (CellQueryP, CellStateP, makeResultsCellComponent, makeQueryPrism, _MarkdownState, _MarkdownQuery)
import SlamData.Notebook.Cell.Markdown.Component.Query (QueryP)
import SlamData.Notebook.Cell.Markdown.Component.State (State, StateP, initialState)
import SlamData.Notebook.Cell.Markdown.Interpret (formFieldValueToLiteral)
import SlamData.Notebook.Cell.Markdown.Model (Model, decode, encode)
import SlamData.Notebook.Cell.Port as Port
import SlamData.Effects (Slam)
import SlamData.Render.CSS as CSS

import Text.Markdown.SlamDown.Html as MD

markdownComponent
  :: CellId
  -> BrowserFeatures
  -> H.Component CellStateP CellQueryP Slam
markdownComponent cellId browserFeatures = makeResultsCellComponent
  { component: H.parentComponent { render: render config, eval, peek: Nothing }
  , initialState: H.parentState initialState
  , _State: _MarkdownState
  , _Query: makeQueryPrism _MarkdownQuery
  }
  where
  config :: MD.SlamDownConfig
  config =
    { formName: "cell-" ++ show (runCellId cellId)
    , browserFeatures
    }

queryShouldRun :: forall a. QueryP a -> Boolean
queryShouldRun = coproduct (const false) (pred <<< H.runChildF)
  where
  pred (MD.TextChanged _ _ _ _) = true
  pred (MD.CheckBoxChanged _ _ _ _) = true
  pred _ = false

render
  :: forall a
   . MD.SlamDownConfig
  -> a
  -> H.ParentHTML MD.SlamDownState CellEvalQuery MD.SlamDownQuery Slam Unit
render config _ =
  HH.div
    [ HP.class_ CSS.markdownOutput ]
    [ HH.slot unit \_ ->
        { component: MD.slamDownComponent config
        , initialState: MD.emptySlamDownState
        }
    ]

formStateToVarMap
  :: MD.SlamDownFormState
  -> Port.VarMap
formStateToVarMap =
  map $ formFieldValueToLiteral >>> Port.Literal

eval
  :: Natural
     CellEvalQuery
     (H.ParentDSL State MD.SlamDownState CellEvalQuery MD.SlamDownQuery Slam Unit)
eval (NotifyRunCell next) = pure next
eval (EvalCell value k) =
  case value.inputPort of
    Just (Port.SlamDown input) -> do
      H.set $ Just input
      H.query unit $ H.action (MD.SetDocument input)
      state <- H.query unit $ H.request MD.GetFormState
      pure $ k case state of
        Nothing -> error "GetFormState query returned Nothing"
        Just st -> { output: Just (Port.VarMap $ formStateToVarMap st), messages: [] }
    _ -> pure $ k (error "expected SlamDown input")
eval (SetupCell _ next) = pure next
eval (Save k) = do
  input <- fromMaybe mempty <$> H.get
  state <- fromMaybe SM.empty <$> H.query unit (H.request MD.GetFormState)
  pure $ k (encode { input, state })
eval (Load json next) = do
  case decode json of
    Right { input, state } ->
      void $ do
        H.set $ Just input
        H.query unit $ H.action (MD.SetDocument input)
        H.query unit $ H.action (MD.PopulateForm state)
    _ -> pure unit
  pure next
eval (SetCanceler _ next) = pure next

error :: String -> CellEvalResult
error msg =
  { output: Nothing
  , messages: [Left $ "An internal error occurred: " ++ msg]
  }
