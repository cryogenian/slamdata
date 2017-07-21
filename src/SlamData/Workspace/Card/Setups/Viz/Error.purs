module SlamData.Workspace.Card.Setups.Viz.Error where

import SlamData.Prelude

import Data.Foldable (intercalate)
import Data.List as L
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as Pr
import Utils (throwVariantError)

type Error =
  { missingProjections ∷ L.List Pr.Projection
  , vizType ∷ VT.VizType
  }

showError ∷ Error → String
showError { missingProjections, vizType } =
  "(MissingAxesError { missingProjections: "
  ⊕ (intercalate ", " (map Pr.print missingProjections))
  ⊕ ", vizType: "
  ⊕ VT.print vizType
  ⊕ "}"

throw
  ∷ ∀ v m a
  . MonadThrow (Variant (setupViz ∷ Error | v)) m
  ⇒ Error
  → m a
throw = throwVariantError (SProxy ∷ SProxy "setupViz")
