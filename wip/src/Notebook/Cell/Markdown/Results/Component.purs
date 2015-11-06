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

import Notebook.Cell.Component (CellQueryP(), CellStateP(), makeResultsCellComponent, _MarkdownState, _MarkdownQuery)
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
  , _Query: _MarkdownQuery <<< C._Right <<< C._Right
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
