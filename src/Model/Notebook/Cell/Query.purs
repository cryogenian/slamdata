module Model.Notebook.Cell.Query where

import Model.Notebook.Cell.JTableContent (JTableContent(), initialJTableContent)
import Optic.Core (Lens(), lens)

type QueryRec =
  { input :: String
  , table :: JTableContent
  }

initialQueryRec :: QueryRec
initialQueryRec =
  { input: ""
  , table: initialJTableContent
  }

_input :: forall a b r. Lens {input :: a | r} {input :: b | r} a b
_input = lens _.input _{input = _}
