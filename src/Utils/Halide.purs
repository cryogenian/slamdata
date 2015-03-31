module Utils.Halide where

import Data.Either
import Halogen.HTML.Events
import Halogen.HTML.Events.Handler
import Halogen.HTML.Events.Types
import qualified Halogen.HTML.Attributes as A

back :: forall e i r. i -> EventHandler (Either i r)
back i = pure $ Left i

request :: forall e i r. r -> EventHandler (Either i r)
request r = pure $ Right r
