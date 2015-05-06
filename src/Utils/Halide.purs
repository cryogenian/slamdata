module Utils.Halide
  ( targetLink
  , targetLink'
  , readonly
  , onPaste
  , onInputMapped
  ) where

import Control.Alternative (Alternative)
import Control.Apply ((*>), (<*))
import Control.Monad.Eff.Class (MonadEff, liftEff)
import Control.MonadPlus (MonadPlus)
import Control.Plus (empty)
import Data.DOM.Simple.Element (setValue)
import Data.DOM.Simple.Types (HTMLElement())
import Data.Either (Either(..))
import Data.Foreign (Foreign(), toForeign)
import Data.Foreign.Class (IsForeign, readProp)
import DOM (DOM())
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.HTML.Events.Types as ET
import qualified Halogen.HTML.Target as T

-- Not sure that this is common case
target' :: forall i m a. (Alternative m) => T.Target (m i) -> [A.Attr (m i)]
target' (T.LinkTarget url) = [ A.href (T.runURL url) ]
target' (T.DataTarget i) = [ A.href "#"
                           , E.onDoubleClick (\_ -> E.stopImmediatePropagation *> pure empty)
                           , E.onClick (\_ -> E.preventDefault *> E.stopPropagation *> pure i)
                           ]

targetLink :: forall i m. (Alternative m) => i -> [A.Attr (m i)]
targetLink = targetLink' <<< pure

targetLink' :: forall i m. (Alternative m) => m i -> [A.Attr (m i)]
targetLink' x = target' (T.DataTarget x)

-- | TODO: remove this once halogen is updated - it now has a readonly attr.
readonly :: forall i. Boolean -> A.Attr i
readonly = A.attr $ A.attributeName "readonly"

-- | Clipboard events actually should have an extended type with a
-- | `clipboardData :: DataTransfer` property, but we don't need that so it is
-- | ignored (for now at least).
onPaste :: forall i. (ET.Event () -> E.EventHandler i) -> A.Attr i
onPaste = A.handler (A.eventName "paste")

onInputMapped :: forall m e i. (MonadPlus m, MonadEff (dom :: DOM | e) m) => (String -> String) -> (String -> i) -> A.Attr (m i)
onInputMapped map f = A.handler (A.eventName "input") (\e -> handler e.target)
  where
  handler :: HTMLElement -> E.EventHandler (m i)
  handler e = case readProp "value" (toForeign e) of
                Left _ -> pure empty
                Right a ->
                  let b = map a
                  in pure $ do
                    liftEff (setValue b e)
                    return $ f b
