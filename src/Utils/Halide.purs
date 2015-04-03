module Utils.Halide where

import Data.Either
import Control.Functor
import Control.Apply
import Halogen.HTML.Events
import Halogen.HTML.Events.Handler
import Halogen.HTML.Events.Types
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Target as T

back :: forall e i r. i -> EventHandler (Either i r)
back i = pure $ Left i

request :: forall e i r. r -> EventHandler (Either i r)
request r = pure $ Right r


-- Not sure that this is common case 
target' :: forall i. T.Target i -> [A.Attr i]
target' (T.LinkTarget url) = [ A.href (T.runURL url) ]
target' (T.DataTarget i) = [ A.href "#",
                             ondblclick (\_ -> stopImmediatePropagation $> i),
                             onclick (\_ -> preventDefault *>
                                            stopPropagation *>
                                            pure i) ]

targetLink :: forall a b. b -> [A.Attr (Either a b)]
targetLink b = target' $ (T.DataTarget <<< Right $ b)
