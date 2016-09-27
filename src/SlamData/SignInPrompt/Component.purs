
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
import Control.Monad.Aff.Promise (Promise)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Events.Indexed as HE

import SlamData.Monad (Slam)
import SlamData.Wiring (Wiring(Wiring), SignInPromptMessage)
import SlamData.Wiring as Wiring

data Query a = Init a | Message SignInPromptMessage a | Dismiss a
type State = Maybe { src ∷ String, dismissed ∷ Promise Unit }

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Slam

initialState ∷ State
initialState = Nothing

comp ∷ H.Component State Query Slam
comp = H.lifecycleComponent { render, eval, initializer: Just $ H.action Init, finalizer: Nothing }

render ∷ State → HTML
render =
  maybe (HH.text "") (HH.div_ ∘ append [ dismissButton ] ∘ pure ∘ iFrameWithSrc ∘ _.src)

dismissButton ∷ HTML
dismissButton = HH.button [ HE.onClick $ HE.input_ Dismiss ] [ HH.text "Dismiss" ]

iFrameWithSrc ∷ ∀ f a. String → HH.HTML a (f Unit)
iFrameWithSrc = HH.iframe ∘ pure ∘ HP.src

eval ∷ Query ~> DSL
eval =
  case _ of
    Init next → pure next
    Message message next →
      case message of
        Wiring.DismissSignInPrompt → ?dismiss
        Wiring.PresentSignInPrompt { src, dismissedAVar } →
          sameSrc src >>=
            if _
              then ?putAVarWhenDismissed dismissedAVar
              else ?dismiss *> ?present src *> ?putAVarWhenDismissed dismissedAVar
        $> next
    Dismiss next → ?dismiss $> next
  where
  runWiring (Wiring wiring) = wiring
  getSignInPromptBus = _.signInPromptBus ∘ runWiring =<< ask
  sameSrc src = maybe false (eq src ∘ _.src) <$> H.get
