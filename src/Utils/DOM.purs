{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Utils.DOM where

import SlamData.Prelude

import Control.Coroutine (Producer)
import Control.Coroutine.Aff as AffCoroutine
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)

import Data.Array (uncons, sort, reverse)
import Data.Nullable (toMaybe)

import DOM (DOM)
import DOM.Event.EventTarget as EventTarget
import DOM.Event.Types (EventTarget, EventType, Event)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes as EventTypes
import DOM.HTML.Types (Window, HTMLElement, htmlElementToElement, htmlDocumentToDocument, windowToEventTarget)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode as P
import DOM.Node.Types (elementToParentNode, Element, documentToEventTarget)

import Unsafe.Coerce (unsafeCoerce)

foreign import waitLoaded ∷ ∀ e. Aff (dom ∷ DOM |e) Unit
foreign import onLoad ∷ ∀ e. Eff e Unit → Eff e Unit
foreign import blur ∷ ∀ e. HTMLElement → Eff (dom ∷ DOM|e) Unit
foreign import focus ∷ ∀ e. HTMLElement → Eff (dom ∷ DOM|e) Unit
foreign import offsetLeft ∷ ∀ e. HTMLElement → Eff (dom ∷ DOM|e) Number
foreign import getBoundingClientRect ∷ ∀ eff.  HTMLElement → Eff (dom ∷ DOM | eff) DOMRect
foreign import getTextWidth ∷ ∀ eff. String → String → Eff (dom ∷ DOM | eff) Number
foreign import elementEq ∷ ∀ eff. HTMLElement → HTMLElement → Eff (dom ∷ DOM | eff) Boolean
foreign import scrollTop ∷ ∀ eff. HTMLElement → Eff (dom ∷ DOM | eff) Number
foreign import scrollLeft ∷ ∀ eff. HTMLElement → Eff (dom ∷ DOM | eff) Number
foreign import getOffsetClientRect ∷ ∀ eff.  HTMLElement → Eff (dom ∷ DOM | eff) DOMRect
foreign import open ∷ ∀ eff. String → String → String → Window → Eff (dom ∷ DOM | eff) Unit
foreign import close ∷ ∀ eff. Window → Eff (dom ∷ DOM | eff) Unit
foreign import centerPopupWindowFeatures ∷ ∀ eff. Int → Int → Window → Eff (dom ∷ DOM | eff) String
foreign import setFontSize ∷ ∀ eff. HTMLElement → String → Eff (dom ∷ DOM | eff) Unit
foreign import getOffsetWidth ∷ ∀ eff. HTMLElement → Eff (dom ∷ DOM | eff) Int
foreign import getOffsetHeight ∷ ∀ eff. HTMLElement → Eff (dom ∷ DOM | eff) Int
foreign import getScrollWidth ∷ ∀ eff. HTMLElement → Eff (dom ∷ DOM | eff) Int
foreign import getScrollHeight ∷ ∀ eff. HTMLElement → Eff (dom ∷ DOM | eff) Int

-- | Same as `getTextWidth` but w/o Eff wrapper. This function definitely has effects
-- | of allocating canvas and should have `Eff (ref ∷ REF|e)` or `Eff (dom ∷ DOM|e)`
-- | but since we don't use intermediate `canvas` anywhere it's safe to think about
-- | this as pure function from font style and string to width.
foreign import getTextWidthPure ∷ String → String → Number

fits ∷ ∀ eff. HTMLElement → Eff (dom ∷ DOM | eff) Boolean
fits el = (&&) <$> fitsHorizontally <*> fitsVertically
  where
  fitsHorizontally = (<=) <$> getScrollWidth el <*> getOffsetWidth el
  fitsVertically = (<=) <$> getScrollHeight el <*> getOffsetHeight el

fitText ∷ ∀ eff. Array Int → HTMLElement → Eff (dom ∷ DOM | eff) (Maybe Int)
fitText fontSizes el = go $ reverse $ sort fontSizes
  where
  go remainingFontSizes = maybe (pure Nothing) f $ uncons remainingFontSizes
  f { head, tail } =
    setFontSize el (g head) *> fits el >>= (if _ then pure (Just head) else go tail)
  g = flip append "px" ∘ show

type DOMRect =
  { left ∷ Number
  , top ∷ Number
  , width ∷ Number
  , height ∷ Number
  }

elementToHTMLElement ∷ Element → HTMLElement
elementToHTMLElement = unsafeCoerce

querySelector
  ∷ ∀ e
  . String
  → HTMLElement
  → Eff (dom ∷ DOM|e) (Maybe HTMLElement)
querySelector str htmlEl =
  map (toMaybe ⋙ map elementToHTMLElement)
  $ P.querySelector str $ elementToParentNode $ htmlElementToElement htmlEl

documentTarget ∷ ∀ e. Eff (dom ∷ DOM|e) EventTarget
documentTarget = htmlDocumentToEventTarget <$> (document =<< window)
  where
  htmlDocumentToEventTarget = documentToEventTarget ∘ htmlDocumentToDocument

onResize ∷ ∀ eff. Eff (dom ∷ DOM | eff) Unit → Eff (dom ∷ DOM | eff) Unit
onResize cb = do
  let listener = EventTarget.eventListener \_ → cb
  window
    >>= windowToEventTarget
    >>> EventTarget.addEventListener EventTypes.resize listener false

eventProducer
  ∷ forall eff
  . EventType
  → Boolean
  → EventTarget
  → Producer Event (Aff (dom ∷ DOM, avar ∷ AVAR | eff)) Unit
eventProducer eventType capture eventTarget =
  AffCoroutine.produce \emit →
    EventTarget.addEventListener
      eventType
      (EventTarget.eventListener $ emit <<< Left)
      capture
      eventTarget

openPopup ∷ ∀ eff. String → Eff (dom ∷ DOM | eff) Unit
openPopup stringUrl = do
  window ← window
  windowFeaturesStr ← centerPopupWindowFeatures 800 600 window
  open stringUrl "SignIn" windowFeaturesStr window
