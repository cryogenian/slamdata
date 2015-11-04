
module Entry.Dashboard where

import Prelude

import Control.Monad.Aff (runAff, forkAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import DOM.BrowserFeatures.Detectors (detectBrowserFeatures)

import Halogen (runUI, installedState)
import Halogen.Util (appendToBody)

import Notebook.Component (notebookComponent, initialNotebook)
import Notebook.Effects (NotebookEffects())
import Notebook.Routing (routeSignal)
import Notebook.Autosave (autoSaveSignal)

main :: Eff NotebookEffects Unit
main = do
  browserFeatures <- detectBrowserFeatures
  runAff throwException (const (pure unit)) $ do
    app <- runUI notebookComponent $ installedState (initialNotebook browserFeatures)
    appendToBody app.node
    forkAff $ routeSignal app.driver
    forkAff $ autoSaveSignal app.driver
