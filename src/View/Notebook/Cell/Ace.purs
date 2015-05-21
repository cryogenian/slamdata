module View.Notebook.Cell.Ace (aceEditor) where

import Optic.Core ((^.))

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

import View.Common (fadeWhen, row)
import qualified View.Css as VC 
import View.Notebook.Common (HTML(), dataCellId, dataCellType)
import Model.Notebook.Cell (Cell(), CellContent(), _hiddenEditor, _cellId, _content, cellContentType)


aceEditor :: forall e. Cell -> HTML e
aceEditor state =
  row [ H.div [ A.classes $ [VC.cellInput] <> fadeWhen (state ^. _hiddenEditor) ]
        [ H.div [ dataCellId $ state ^. _cellId
                , dataCellType $ state ^. _content
                , A.classes [ VC.aceContainer ]
                ]
          []
        ]
      ]
