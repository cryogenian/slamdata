module SlamData.Workspace.Card.BuildChart.PunchCard.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.PunchCard.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray)

import ECharts.Monad (DSL)
import ECharts.Types.Phantom (OptionI)

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL )
import SlamData.Quasar.Error as QE
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.CardType.ChartType (ChartType(PunchCard))
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.BuildChart.PunchCard.Model (PunchCardR, Model)

eval
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ Model
  → FilePath
  → CET.CardEvalT m Port.Port
eval Nothing _ =
  QE.throw "Please select axis to aggregate"
eval (Just conf) resource = do
  records ← BCE.records resource
  pure $ Port.ChartInstructions (buildPunchCard conf records) PunchCard


buildPunchCard ∷ PunchCardR → JArray → DSL OptionI
buildPunchCard r records = pure unit
