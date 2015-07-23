module View.Notebook.Common where

import Prelude
import Controller.Notebook (I())
import EffectTypes (NotebookAppEff())
import Halogen.HTML.Attributes (Attr(), attr, attributeName)
import Halogen.HTML.Events.Monad (Event())
import Input.Notebook (Input())
import Model.Notebook.Cell (CellContent(), cellContentType, CellId())
import qualified Halogen.HTML as H

type HTML e = H.HTML (I e)

dataCellId :: forall i. CellId -> Attr i
dataCellId = attr (attributeName "data-cell-id") <<< show

dataCellType :: forall i. CellContent -> Attr i
dataCellType = attr (attributeName "data-cell-type") <<< cellContentType

dataEChartsId :: forall i. String -> Attr i
dataEChartsId = attr (attributeName "data-echarts-id")
