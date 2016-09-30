
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

module SlamData.SignInPrompt.Component where

import SlamData.Prelude

import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Promise (Promise)
import Control.Monad.Aff.Promise as Promise

import Halogen as H
import Halogen.Component.Utils (subscribeToBus)
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Events.Indexed as HE

import SlamData.Monad (Slam)
import SlamData.Wiring (Wiring(Wiring), SignInPromptMessage)
import SlamData.Wiring as Wiring

data Query a = Init a | Message SignInPromptMessage a | Dismiss a
type PromiseVar = { input ∷ AVar Unit, output ∷ Promise Unit }
type State = Maybe { src ∷ String, dismissed ∷ PromiseVar }

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Slam

initialState ∷ State
initialState = Nothing

comp ∷ H.Component State Query Slam
comp =
  H.lifecycleComponent
    { render, eval, initializer: Just $ H.action Init, finalizer: Nothing }

render ∷ State → HTML
render =
  maybe
    (HH.text "")
    (HH.div_ ∘ append [ dismissButton ] ∘ pure ∘ iFrameWithSrc ∘ _.src)

dismissButton ∷ HTML
dismissButton = HH.button [ HE.onClick $ HE.input_ Dismiss ] [ HH.text "Dismiss" ]

iFrameWithSrc ∷ ∀ f a. String → HH.HTML a (f Unit)
iFrameWithSrc = HH.iframe ∘ pure ∘ HP.src

eval ∷ Query ~> DSL
eval =
  case _ of
    Init next →
      (getSignInPromptBus >>= subscribeToBus (H.action ∘ Message)) $> next
    Message message next →
      case message of
        Wiring.DismissSignInPrompt → dismiss
        Wiring.PresentSignInPrompt { src, dismissedAVar } →
          H.get >>=
            case _ of
              Nothing → present src >>= putAVarWhenDismissed dismissedAVar
              Just r →
                if r.src == src
                  then putAVarWhenDismissed dismissedAVar r.dismissed
                  else dismiss *> present src >>= putAVarWhenDismissed dismissedAVar
        $> next
    Dismiss next → dismiss $> next
  where
  runWiring (Wiring wiring) = wiring
  getSignInPromptBus ∷ DSL (BusRW Wiring.SignInPromptMessage)
  getSignInPromptBus = _.signInPromptBus ∘ runWiring <$> H.liftH ask

putAVarWhenDismissed ∷ AVar Unit → PromiseVar → DSL Unit
putAVarWhenDismissed aVar dismissed =
  H.fromAff $ AVar.putVar aVar =<< Promise.wait dismissed.output

dismiss ∷ DSL Unit
dismiss = H.get >>= maybe (pure unit) (dismiss' ∘ _.dismissed)

dismiss' ∷ PromiseVar → DSL Unit
dismiss' dismissed = (H.fromAff $ AVar.putVar dismissed.input unit) *> H.set Nothing

present ∷ String → DSL PromiseVar
present src = do
  input ← H.fromAff AVar.makeVar
  output ← H.fromAff $ Promise.defer $ AVar.takeVar input
  let dismissed = { input, output }
  H.set $ Just { src, dismissed }
  pure dismissed
