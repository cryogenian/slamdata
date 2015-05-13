module View.Notebook.Cell.Explore (exploreEditor, exploreOutput) where

import Model.Notebook.Cell.Explore
import View.Notebook.Common
import qualified Halogen.HTML as H

exploreEditor :: forall e. HTML e
exploreEditor = H.div_ []

exploreOutput :: forall e. ExploreRec -> [HTML e]
exploreOutput = const []
