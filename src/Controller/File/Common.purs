module Controller.File.Common where

import Control.Monad.Eff (Eff())
import Data.DOM.Simple.Encode (encodeURIComponent)
import Data.Inject1 (Inject1, inj)
import Data.String (joinWith)
import DOM (DOM())
import Model.File.Item (Item(), _resource)
import Model.Path (encodeURIPath)
import Model.Resource (_path, isFile, resourcePath)
import Optic.Core ((^.))
import Utils (newTab)

-- | Lifts an input value into an applicative and injects it into the right
-- | place in an Either.
toInput :: forall m a b. (Applicative m, Inject1 a b) => a -> m b
toInput = pure <<< inj

-- | Open a notebook or file.
open :: forall e. Item -> Eff (dom :: DOM | e) Unit
open item = newTab $ joinWith "" $ [ Config.notebookUrl
                                   , "#"
                                   , encodeURIPath $ resourcePath $ item ^. _resource
                                   , if isFile item.resource then "/explore" else "edit"
                                   ]
