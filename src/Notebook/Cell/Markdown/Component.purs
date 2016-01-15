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

module Notebook.Cell.Markdown.Component
  ( markdownComponent
  , queryShouldRun
  , module Notebook.Cell.Markdown.Component.Query
  , module Notebook.Cell.Markdown.Component.State
  ) where

import Prelude

import Control.Bind ((=<<))

import Data.BrowserFeatures (BrowserFeatures())
import Data.Either (Either(..))
import Data.Functor.Coproduct (coproduct)
import Data.Lens (preview)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.StrMap as SM

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Text.Markdown.SlamDown.Html (SlamDownConfig(), SlamDownState(), SlamDownQuery(..), slamDownComponent, emptySlamDownState)

import Render.CssClasses as CSS

import Notebook.Cell.CellId (CellId(), runCellId)
import Notebook.Cell.Port (Port(..), _SlamDown)
import Notebook.Cell.Common.EvalQuery (CellEvalQuery(..), CellEvalResult())
import Notebook.Cell.Component (CellQueryP(), CellStateP(), makeResultsCellComponent, makeQueryPrism, _MarkdownState, _MarkdownQuery)
import Notebook.Cell.Markdown.Component.Query
import Notebook.Cell.Markdown.Component.State
import Notebook.Cell.Markdown.Model
import Notebook.Common (Slam())

markdownComponent
  :: CellId
  -> BrowserFeatures
  -> Component CellStateP CellQueryP Slam
markdownComponent cellId browserFeatures = makeResultsCellComponent
  { component: parentComponent (render config) eval
  , initialState: installedState initialState
  , _State: _MarkdownState
  , _Query: makeQueryPrism _MarkdownQuery
  }
  where
  config :: SlamDownConfig
  config =
    { formName: "cell-" ++ show (runCellId cellId)
    , browserFeatures
    }

queryShouldRun :: forall a. QueryP a -> Boolean
queryShouldRun = coproduct (const false) (\(ChildF _ q) -> pred q)
  where
  pred (TextChanged _ _ _ _) = true
  pred (CheckBoxChanged _ _ _ _) = true
  pred _ = false

render
  :: forall a
   . SlamDownConfig
  -> a
  -> ParentHTML SlamDownState CellEvalQuery SlamDownQuery Slam Unit
render config _ =
  H.div
    [ P.class_ CSS.markdownOutput ]
    [ H.slot unit \_ ->
        { component: slamDownComponent config
        , initialState: emptySlamDownState
        }
    ]

eval
  :: Natural
     CellEvalQuery
     (ParentDSL State SlamDownState CellEvalQuery SlamDownQuery Slam Unit)
eval (NotifyRunCell next) = pure next
eval (EvalCell value k) =
  case preview _SlamDown =<< value.inputPort of
    Just input -> do
      set $ Just input
      query unit $ action (SetDocument input)
      state <- query unit $ request GetFormState
      pure $ k case state of
        Nothing -> error "GetFormState query returned Nothing"
        Just st -> { output: Just (VarMap st), messages: [] }
    Nothing -> pure $ k (error "expected SlamDown input")
eval (Save k) = do
  input <- fromMaybe mempty <$> get
  state <- fromMaybe SM.empty <$> query unit (request GetFormState)
  pure $ k (encode { input, state })
eval (Load json next) = do
  case decode json of
    Right { input, state } ->
      void $ do
        set $ Just input
        query unit $ action (SetDocument input)
        query unit $ action (PopulateForm state)
    _ -> pure unit
  pure next

error :: String -> CellEvalResult
error msg =
  { output: Nothing
  , messages: [Left $ "An internal error occurred: " ++ msg]
  }
