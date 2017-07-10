module SlamData.Workspace.Card.CardType.VizType where

import SlamData.Prelude

import SlamData.Workspace.Card.CardType.Chart as Cht
import SlamData.Workspace.Card.CardType.Geo as Geo
import SlamData.Workspace.Card.CardType.Input as Inp
import SlamData.Workspace.Card.CardType.Select as Sel
import SlamData.Workspace.Card.CardType.Static as Sta

type VizType =
  Variant ( Cht.ChartR ( Geo.GeoR ( Sel.SelectR ( Inp.InputR ( Sta.StaticR () ) ) ) ) )

all ∷ Array VizType
all = Cht.all ⊕ Geo.all ⊕ Inp.all ⊕ Sel.all ⊕ Sta.all
