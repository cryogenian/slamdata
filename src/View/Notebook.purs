module View.Notebook (view) where

import Data.List (toArray)
import Model.Notebook
import Model.Action
import qualified Halogen.HTML as H

view :: forall p i. State -> H.HTML p i
view (OneCellView resume cellState) = viewCell resume cellState
view (NotebookView resume id cells) =
    H.div_ $
      [ H.p_ [ H.text ("notebook path: " ++ id) ]
      , H.p_ [ H.text "cells in notebook: " ]
      ] ++ toArray (viewCell resume <$> cells)

viewCell :: forall p i. Action -> CellState -> H.HTML p i
viewCell resume state =
  H.div_ $
    editField resume ++
    [ H.pre_ [ H.text state.id ] ]

editField :: forall p i. Action -> [H.HTML p i]
editField Edit = [ H.p_ [ H.text "cell means to be editable" ] ]
editField _    = []
