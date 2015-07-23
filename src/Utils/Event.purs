module Utils.Event (raiseEvent) where

import DOM (DOM())
import Data.Function(Fn2(), runFn2)
import Control.Monad.Eff (Eff())
import Data.DOM.Simple.Types (HTMLElement())

foreign import raiseEventImpl :: forall e a.
                                 Fn2 String HTMLElement
                                 (Eff (dom :: DOM |e) HTMLElement)
                                 
                                 
raiseEvent :: forall e a. String -> HTMLElement -> Eff (dom :: DOM |e) HTMLElement
raiseEvent name el = runFn2 raiseEventImpl name el 
