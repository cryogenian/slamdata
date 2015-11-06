
module Entry.Dashboard where

import Prelude

import Control.Monad.Aff (runAff, forkAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import DOM.BrowserFeatures.Detectors (detectBrowserFeatures)

import Halogen (runUI, installedState)
import Halogen.Util (appendToBody)
import Dashboard.Effects
import Dashboard as Dashboard

main :: Eff DashboardEffects Unit
main = do
  browserFeatures <- detectBrowserFeatures
  runAff throwException (const (pure unit)) $ do
    app <- runUI Dashboard.comp $ installedState (Dashboard.initialState)
    appendToBody app.node
--    forkAff $ routeSignal app.driver
--    forkAff $ autoSaveSignal app.driver
