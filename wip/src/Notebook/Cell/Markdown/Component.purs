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
  , module Notebook.Cell.Markdown.Component.Query
  , module Notebook.Cell.Markdown.Component.State
  ) where

import Prelude

import Control.Bind ((=<<))

import Data.BrowserFeatures (BrowserFeatures())
import Data.Either (Either(..))
import Data.Lens (preview)
import Data.Maybe (Maybe(..))

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Text.Markdown.SlamDown.Html (SlamDownConfig(), SlamDownState(), SlamDownQuery(..), slamDownComponent, emptySlamDownState)

import Render.CssClasses as CSS

import Model.CellId (CellId(), runCellId)
import Model.Port (Port(..), _SlamDown)
import Notebook.Cell.Common.EvalQuery (CellEvalQuery(..), CellEvalResult())
import Notebook.Cell.Component (CellQueryP(), CellStateP(), makeResultsCellComponent, makeQueryPrism, _MarkdownState, _MarkdownQuery)
import Notebook.Cell.Markdown.Component.Query
import Notebook.Cell.Markdown.Component.State
import Notebook.Common (Slam())

markdownComponent
  :: CellId -> BrowserFeatures -> Component CellStateP CellQueryP Slam
markdownComponent cellId browserFeatures = makeResultsCellComponent
  { component: parentComponent render eval
  , initialState: installedState config
  , _State: _MarkdownState
  , _Query: makeQueryPrism _MarkdownQuery
  }
  where
  config :: SlamDownConfig
  config =
    { formName: "cell-" ++ show (runCellId cellId)
    , browserFeatures: browserFeatures
    }

render
  :: SlamDownConfig -> ParentHTML SlamDownState CellEvalQuery SlamDownQuery Slam Unit
render config =
  H.div
    [ P.class_ CSS.markdownOutput ]
    [ H.slot unit \_ ->
        { component: slamDownComponent config
        , initialState: emptySlamDownState
        }
    ]

eval
  :: Natural CellEvalQuery
     (ParentDSL SlamDownConfig SlamDownState CellEvalQuery SlamDownQuery Slam Unit)
eval (NotifyRunCell next) = pure next
eval (EvalCell value k) =
  case preview _SlamDown =<< value.inputPort of
    Just slamdown -> do
      query unit $ action (SetDocument slamdown)
      state <- query unit $ request GetFormState
      pure $ k case state of
        Nothing -> error "GetFormState query returned Nothing"
        Just st -> { output: Just (VarMap st), messages: [] }
    Nothing -> pure $ k (error "expected SlamDown input")

error :: String -> CellEvalResult
error msg =
  { output: Nothing
  , messages: [Left $ "An internal error occurred: " ++ msg]
  }
