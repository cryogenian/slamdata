module View.Notebook.Cell.Search (
  searchEditor,
  searchOutput
  ) where

import Prelude
import Data.Functor (($>))
import Control.Plus (empty)
import Controller.Notebook.Cell (requestCellContent)
import Controller.Notebook.Cell.Search (runSearch)
import Input.Notebook (Input(..))
import Model.Notebook (_notebook)
import Model.Notebook.Cell (Cell(), RunState(..), _FileInput, _JTableContent, _content, _Search, _cellId, _runState, _input)
import Model.Notebook.Cell.Search (_buffer)
import Model.Notebook.Domain (_activeCellId, Notebook())
import Optic.Core 
import Optic.Extended (TraversalP())
import View.Common (glyph)
import View.Notebook.Cell.FileInput (fileInput)
import View.Notebook.Cell.JTableCell (renderJTableOutput)
import View.Notebook.Common (HTML())

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as Vc

searchEditor :: forall e. Notebook -> Cell -> HTML e
searchEditor notebook cell =
  H.div
  [ A.classes [ Vc.exploreCellEditor ] ] $
  (fileInput (_content .. _FileInput) (\port -> _input .~ port) cell) <>
  [ H.div [ A.classes [ Vc.fileListField, B.inputGroup ] ]
    [ H.input
      [ A.value (cell ^. _lens)
      , A.classes [ B.formControl, Vc.searchCellInput ]
      , A.placeholder "Input search string"
      , E.onInput (E.input updateBuffer)
      , E.onFocus (E.input_ $ WithState (_notebook .. _activeCellId .~ (cell ^. _cellId)))
      ] [ ]
    , H.img [ E.onClick (E.input_ $ updateBuffer "")
            , A.class_ Vc.searchClear
            , A.src (case cell ^. _runState of
                        RunningSince _ -> "img/spin.svg"
                        _ -> "img/remove.svg")
            ] [ ]
    , H.span [ A.classes [ B.inputGroupBtn ] ]
      [ H.button [ A.classes [ B.btn, B.btnDefault, Vc.searchCellButton]
                 , A.type_ "button"
                 , E.onClick \_ -> pure (requestCellContent notebook cell)
                 ] [ glyph B.glyphiconSearch ]
      ]
    ]
  ]
  where
  updateBuffer :: String -> Input
  updateBuffer v = UpdateCell (cell ^. _cellId) (_lens .~ v)

_lens :: TraversalP Cell String
_lens = _content .. _Search .. _buffer

searchOutput :: forall e. Cell -> Array (HTML e)
searchOutput = renderJTableOutput (_content .. _JTableContent) runSearch

