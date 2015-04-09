module View.Notebook (view) where

import Data.Either
import Data.List

import qualified Halogen.HTML as Ht
import qualified Model.Notebook as M
import qualified Model.Resume as M

view :: forall u node. (Ht.HTMLRepr node) => M.State ->
        node u (Either M.Input M.Request)
view state =
  case state of
    M.OneCellView resume cellState -> viewCell resume cellState
    M.NotebookView resume id cells ->
      Ht.div_ $
      [Ht.p_ [Ht.text $ "notebook path: " <> id],
       Ht.p_ [Ht.text $ "cells in notebook: "]] <>
      (toArray (viewCell resume <$> cells))


viewCell :: forall u node. (Ht.HTMLRepr node) => M.Resume -> M.CellState ->
            node u (Either M.Input M.Request)
viewCell resume state =
  Ht.div_ $ 
  editField resume <> 
  [Ht.pre_ [Ht.text state.id]]
  
  where editField resume =
          if resume == M.Edit then
            [Ht.p_ [Ht.text "cell means to be editable"]]
          else []
