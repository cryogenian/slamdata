module Dashboard.Cell.Def (CellPart(..), Def()) where

import Prelude

import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)
import Data.Lens (APrismP())

import Halogen (Component())
import Halogen.HTML.Indexed as H

import Dashboard.Cell.Query
import Dashboard.Cell.State
import Dashboard.Cell.Common.EditorQuery
import Dashboard.Cell.Common.ResultsQuery
import Dashboard.Common (Slam())

data CellPart = EditorPart | ResultsPart
derive instance genericCellPart :: Generic CellPart
instance eqAnyCellSlot :: Eq CellPart where eq = gEq
instance ordAnyCellSlot :: Ord CellPart where compare = gCompare

type Def se fe sr fr =
  { name :: String
  , glyph :: H.ClassName
  , editor :: Component se (Coproduct CellEditorQuery fe) Slam
  , editorState :: se
  , _StateE :: APrismP AnyCellState se
  , _QueryE :: forall a. APrismP (AnyCellQuery a) (fe a)
  , results :: Component sr (Coproduct CellResultsQuery fr) Slam
  , resultsState :: sr
  , _StateR :: APrismP AnyCellState sr
  , _QueryR :: forall a. APrismP (AnyCellQuery a) (fr a)
  }
