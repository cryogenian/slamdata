module Controller.File.Common where

import Data.Inject1 (Inject1, inj)
import Control.Monad.Eff (Eff())
import Data.String (joinWith)
import DOM (DOM())
import Model.File.Item (Item(), resourceL)
import Model.Path (encodeURIPath)
import Model.Resource (resourcePath)
import Utils (newTab, encodeURIComponent)
import Optic.Core 

-- | Lifts an input value into an applicative and injects it into the right
-- | place in an Either.
toInput :: forall m a b. (Applicative m, Inject1 a b) => a -> m b
toInput = pure <<< inj

-- | Open a notebook or file.
open :: forall e. Item -> Boolean -> Eff (dom :: DOM | e) Unit
open item isNew = 
  newTab $ joinWith "" $
    [Config.notebookUrl, "#", (encodeURIPath $ resourcePath $ item ^. resourceL), "/edit"]
    <> if isNew then ["/?q=", encodeURIComponent ("select * from ...")] else []
