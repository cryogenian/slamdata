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

module Notebook.Cell.Markdown.Results.Component
  ( markdownResultsComponent
  , module Notebook.Cell.Markdown.Results.Component.Query
  , module Notebook.Cell.Markdown.Results.Component.State
  ) where

import Prelude

import Control.Bind ((=<<))

import Data.Either (Either(..))
import Data.Lens (preview)
import Data.Lens.Prism.Coproduct as C
import Data.Lens.Prism.Either as E
import Data.Maybe (Maybe(..))
import Data.StrMap as SM

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Text.Markdown.SlamDown.Html (SlamDownConfig(), SlamDownState(), SlamDownQuery(..), slamDownComponent, emptySlamDownState)

import Render.CssClasses as CSS

import Notebook.Cell.Component (CellQueryP(), CellStateP(), makeResultsCellComponent, makeQueryPrism, _MarkdownState, _MarkdownQuery)
import Notebook.Cell.Common.EvalQuery (CellEvalQuery(..))
import Notebook.Cell.Markdown.Results.Component.Query
import Notebook.Cell.Markdown.Results.Component.State
import Notebook.Cell.Port (Port(..), _SlamDown)
import Notebook.Common (Slam())

markdownResultsComponent :: SlamDownConfig -> Component CellStateP CellQueryP Slam
markdownResultsComponent config = makeResultsCellComponent
  { component: parentComponent render eval
  , initialState: installedState config
  , _State: _MarkdownState <<< E._Right
  , _Query: makeQueryPrism (_MarkdownQuery <<< C._Right)
  }

render :: SlamDownConfig -> ParentHTML SlamDownState CellEvalQuery SlamDownQuery Slam Unit
render config =
  H.div
    [ P.class_ CSS.markdownOutput ]
    [ H.slot unit \_ -> { component: slamDownComponent config, initialState: emptySlamDownState } ]

eval :: Natural CellEvalQuery (ParentDSL SlamDownConfig SlamDownState CellEvalQuery SlamDownQuery Slam Unit)
eval (EvalCell value k) = do
  case preview _SlamDown =<< value of
    Just slamdown -> do
      query unit $ action (SetDocument slamdown)
      pure $ k
        { output: Just $ VarMap SM.empty -- TODO: use real varmap
        , messages: [] -- TODO: describe output fields
        }
    Nothing ->
      pure $ k
        { output: Nothing
        , messages: [Left "Expected SlamDown input"]
        }
