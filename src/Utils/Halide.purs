module Utils.Halide
  ( onPaste
  , width'
  , height'
  , frameBorder
  , max
  , min
  , step
  , selectThis
  , dataZeroClipboard
  ) where

import Control.Apply ((*>))
import Control.Monad.Eff.Class (liftEff)
import Control.Plus (empty)
import DOM (DOM())
import Utils (select)

import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.HTML.Events.Types as ET
import qualified Halogen.HTML.Target as T

-- | Clipboard events actually should have an extended type with a
-- | `clipboardData :: DataTransfer` property, but we don't need that so it is
-- | ignored (for now at least).
onPaste :: forall i. (ET.Event () -> E.EventHandler i) -> A.Attr i
onPaste = A.handler (A.eventName "paste")

width' :: forall i. String -> A.Attr i
width' = A.attr (A.attributeName "width")

height' :: forall i. String -> A.Attr i
height' = A.attr (A.attributeName "height")

frameBorder :: forall i. Number -> A.Attr i
frameBorder = A.attr (A.attributeName "frameBorder") <<< show

step :: forall i. Number -> A.Attr i
step = A.attr (A.attributeName "step") <<< show

max :: forall i. Number -> A.Attr i
max = A.attr (A.attributeName "max") <<< show

min :: forall i. Number -> A.Attr i
min = A.attr (A.attributeName "min") <<< show

selectThis :: forall e o i. ET.Event o -> E.EventHandler (E.Event (dom :: DOM | e) i)
selectThis ev = pure $ liftEff (select ev.target) *> empty

dataZeroClipboard :: forall i. String -> A.Attr i
dataZeroClipboard content = A.attr (A.attributeName "data-zclipboard") content
