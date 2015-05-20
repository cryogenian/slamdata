module View.Notebook.Cell.JTableCell (renderJTableOutput) where

import Control.Functor (($>))
import Controller.Notebook.Cell.JTableContent
import Controller.Notebook.Common (I())
import Data.Int (toNumber)
import Data.Json.JTable (renderJTable, jTableOptsDefault, bootstrapStyle)
import Data.Maybe (fromMaybe)
import Data.These (these, theseRight)
import Data.Void (absurd)
import Model.Notebook.Cell (Cell())
import Model.Notebook.Cell.JTableContent (JTableContent(), _result, _page, _values, _totalPages)
import Optic.Core ((^.), (..))
import Optic.Extended (TraversalP(), (^?))
import Optic.Refractor.Prism (_Just)
import View.Common (glyph)
import View.Notebook.Common (HTML())

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as VC

renderJTableOutput :: forall e. TraversalP Cell JTableContent -> (Cell -> I e) -> Cell -> [HTML e]
renderJTableOutput lens run cell = fromMaybe [] $ do
  table <- cell ^? lens
  result <- table ^? _result .. _Just
  json <- result ^? _values .. _Just
  let totalPages = result ^. _totalPages
      page = fromMaybe one $ theseRight (table ^. _page)
      output = renderJTable (jTableOptsDefault { style = bootstrapStyle }) json
  return
    [ absurd <$> output
    , H.div [ A.classes [B.btnGroup] ]
            [ H.button [ A.classes [B.btn, B.btnSm, B.btnDefault]
                       , A.disabled (page <= one)
                       , E.onClick (\_ -> pure $ goPage one cell run)
                       ]
                       [ glyph B.glyphiconFastBackward ]
            , H.button [ A.classes [B.btn, B.btnSm, B.btnDefault]
                       , A.disabled (page <= one)
                       , E.onClick (\_ -> pure $ stepPage (-one) cell run)
                       ]
                       [ glyph B.glyphiconStepBackward ]
            ]
    , H.div [ A.classes [VC.pageInput] ]
            [ H.form [ E.onSubmit (\_ -> E.preventDefault $> loadPage cell run) ]
                     [ H.text "Page"
                     , H.input [ A.classes [B.formControl, B.inputSm]
                               , A.value (these id (show <<< toNumber) (\s _ -> s) (table ^. _page))
                               , E.onInput (pure <<< updatePage cell)
                               ]
                               []
                     , H.text $ "of " ++ (show $ toNumber totalPages)
                     ]
            ]
    , H.div [ A.classes [B.btnGroup] ]
            [ H.button [ A.classes [B.btn, B.btnSm, B.btnDefault]
                       , A.disabled (page >= totalPages)
                       , E.onClick (\_ -> pure $ stepPage one cell run)
                       ]
                       [ glyph B.glyphiconStepForward ]
            , H.button [ A.classes [B.btn, B.btnSm, B.btnDefault]
                       , A.disabled (page >= totalPages)
                       , E.onClick (\_ -> pure $ goPage totalPages cell run)
                       ]
                       [ glyph B.glyphiconFastForward ]
            ]
    , H.div [ A.classes [VC.pageSize] ]
            [ H.text "Per page:"
            , H.select [ A.classes [B.formControl, B.inputSm]
                       , E.onValueChanged (pure <<< changePageSize cell run)
                       ]
                       [ H.option_ [ H.text "10" ]
                       , H.option_ [ H.text "25" ]
                       , H.option_ [ H.text "50" ]
                       , H.option_ [ H.text "100" ]
                       ]
            ]
    ]
