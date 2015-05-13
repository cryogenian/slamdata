module View.Notebook.Cell.Explore (exploreEditor, exploreOutput) where

import View.Notebook.Common
import qualified Halogen.HTML as H

exploreEditor :: forall e. HTML e
exploreEditor = H.div_ []

exploreOutput :: forall e. String -> [HTML e]
exploreOutput = const []
