module Utils.Halide
  ( targetLink
  , targetLink'
  , readonly
  ) where

import Data.Either
import Control.Functor
import Control.Apply
import Control.Plus (empty)
import Control.Alternative (Alternative)
import Halogen.HTML.Events
import Halogen.HTML.Events.Handler
import Halogen.HTML.Events.Types
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Target as T

-- Not sure that this is common case
target' :: forall i m a. (Alternative m) => T.Target (m i) -> [A.Attr (m i)]
target' (T.LinkTarget url) = [ A.href (T.runURL url) ]
target' (T.DataTarget i) = [ A.href "#"
                           , onDoubleClick (\_ -> stopImmediatePropagation *> pure empty)
                           , onClick (\_ -> preventDefault *> stopPropagation *> pure i)
                           ]

targetLink :: forall i m. (Alternative m) => i -> [A.Attr (m i)]
targetLink = targetLink' <<< pure

targetLink' :: forall i m. (Alternative m) => m i -> [A.Attr (m i)]
targetLink' x = target' (T.DataTarget x)

readonly :: forall i. Boolean -> A.Attr i
readonly = A.attr $ A.attributeName "readonly"
