module SlamData.Workspace.Card.Setups.Viz.Error where

import SlamData.Prelude

import Data.Foldable (intercalate)

import Data.List as L
import SlamData.Workspace.Card.Setups.Package.Types as T
import SlamData.Workspace.Card.Setups.Package.Projection as P
import Utils (throwVariantError)

data Error
  = MissingAxesError (L.List T.Projection)

instance showSetupVizError ∷ Show Error where
  show = case _ of
    MissingAxesError prjs →
      "(MissingAxesError " <> intercalate ", " (L.catMaybes $ map P.printProjection prjs) <> ")"

throw
  ∷ ∀ v m a
  . MonadThrow (Variant (setupViz ∷ Error | v)) m
  ⇒ Error
  → m a
throw = throwVariantError (SProxy ∷ SProxy "setupViz")

_setupVizError = SProxy ∷ SProxy "setupViz"
