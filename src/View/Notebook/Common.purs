module View.Notebook.Common (HTML(), dataCellId, dataCellType, dataEChartsId) where

import Input.Notebook (Input())
import EffectTypes (NotebookAppEff())
import Halogen.HTML.Events.Monad (Event())
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import Model.Notebook.Cell (CellContent(), cellContentType)

type HTML e = H.HTML (Event (NotebookAppEff e) Input)


dataCellId :: forall i. Number -> A.Attr i
dataCellId = A.attr (A.attributeName "data-cell-id")

dataCellType :: forall i. CellContent -> A.Attr i
dataCellType = A.attr (A.attributeName "data-cell-type") <<< cellContentType

dataEChartsId :: forall i. String -> A.Attr i
dataEChartsId = A.attr (A.attributeName "data-echarts-id") 
