module View.Notebook.Cell.Viz (vizChoices, vizOutput) where

import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.First (runFirst, First())
import qualified Data.StrMap as M 
import Optic.Core ((^.), (..))
import Optic.Fold ((^?))
import Optic.Extended (TraversalP())
import Data.Array (range, zipWith, length, drop, take)


import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Halogen.HTML.CSS as CSS

import Css.Size
import Css.Geometry
import Css.String

import ECharts.Options

import Model.Notebook.Cell (Cell(), _cellId, _content, _Visualize)
import Model.Notebook.Cell.Viz (VizRec(), _miniatures, _error, _baseSelected, _baseOptions, miniatureLabel, baseOptsLabel, resultLabel)
import Controller.Notebook.Common (I())
import Controller.Notebook.Cell.Viz (previewSelected)
import View.Common (row)
import qualified View.Css as VC 
import View.Notebook.Common (HTML(), dataCellId, dataCellType, dataEChartsId)

vizChoices :: forall e. Cell -> HTML e
vizChoices cell =
  maybe go errored (runFirst $ cell ^. _err)
  where
  go = if (cell ^? _isBase) == Just false
       then  H.div_ [ row $ bases' [ A.classes [ B.colXs3 ] ] cell ]
       else advancedChoices cell

advancedChoices :: forall e. Cell -> HTML e
advancedChoices cell =
  row
  [ H.div [ A.classes [ B.colXs3 ] ]
    (bases cell)
  , H.div [ A.classes [ B.colXs9 ] ]
    [ row (rowMinis cell)
    ]
  ]

errored :: forall e. String -> HTML e
errored message =
  H.div [ A.classes [ B.alert, B.alertDanger ]
        , CSS.style do
             marginBottom $ px 12
        ]
  [ H.text message ]
  

vizOutput :: forall e. Cell -> [ HTML e ]
vizOutput state =
  maybe [chart ((show $ state ^. _cellId) <> resultLabel) 300]
  (const []) (runFirst $ state ^._err)



bases' :: forall e. [A.Attr (I e)] -> Cell -> [ HTML e ]
bases' = miniatures (^. _baseOpts) baseOptsLabel 100

bases :: forall e. Cell -> [ HTML e ]
bases = miniatures (^. _baseOpts) baseOptsLabel 100 [ ]

rowMinis :: forall e. Cell -> [ HTML e ]
rowMinis =
  miniatures
  (^. _mini)
  miniatureLabel 100 [A.classes [ B.colXs2 ] ]


miniatures :: forall e. (Cell -> [Option]) -> String -> Number ->
              [A.Attr (I e)] -> Cell -> [ HTML e ]
miniatures fn key height attrs cell = 
  zipWith miniature opts ixs
  where
  cellId :: String
  cellId = show $ cell ^. _cellId

  opts :: [ Option ]
  opts = fn cell

  ixs :: [Number]
  ixs = range 0 $ length opts

  miniature :: Option -> Number -> HTML e
  miniature opt ix =
    H.div attrs 
    [ chart' [ E.onClick (\_ -> pure $ previewSelected cell opt) ]
      (cellId <> key <> show ix) height ]

  
chart' :: forall e i. [A.Attr (I e)] -> String -> Number -> HTML e
chart' attrs chartId h =
  H.div (attrs <>
         [ dataEChartsId chartId
         , A.key ("echarts-key" <> chartId)
         , CSS.style (height $ px h)
         ]) [ ]

chart :: forall e. String -> Number -> HTML e
chart = chart' []



_viz :: TraversalP Cell VizRec
_viz = _content .. _Visualize 

_mini :: TraversalP Cell [Option]
_mini = _viz .. _miniatures

_err :: TraversalP Cell (First String)
_err = _viz .. _error

_isBase :: TraversalP Cell Boolean
_isBase = _viz .. _baseSelected

_baseOpts :: TraversalP Cell [Option]
_baseOpts = _viz .. _baseOptions
