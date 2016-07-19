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

module SlamData.Notification.Component
  ( State
  , Query(..)
  , comp
  , initialState
  ) where

import SlamData.Prelude
import SlamData.Effects (Slam)
import SlamData.Notification as N

import Control.Monad.Aff (later', forkAff)
import Control.Monad.Aff.AVar (AVar, makeVar, takeVar, putVar)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Rec.Class (forever)

import Data.Array as Array
import Data.Int as Int
import Data.Time (Milliseconds(..))

import Halogen as H
import Halogen.Component.Utils (raise)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

type State =
  { tick ∷ Int
  , queue ∷ Array ((AVar Unit) × N.NotificationOptions)
  , current ∷ Maybe ((AVar Unit) × N.Notification)
  , dismissed ∷ Maybe ((AVar Unit) × N.Notification)
  }

initialState ∷ State
initialState =
  { tick: 0
  , queue: []
  , current: Nothing
  , dismissed: Nothing
  }

data Query a
  = Init a
  | Push N.NotificationOptions a
  | Dismiss (AVar Unit) a

data Status
  = Current
  | Dismissed

type NotifyHTML = H.ComponentHTML Query

type NotifyDSL = H.ComponentDSL State Query Slam

comp
  ∷ ∀ r
  . Bus.Bus (read ∷ Bus.Cap | r) N.NotificationOptions
  → H.Component State Query Slam
comp bus =
  H.lifecycleComponent
    { render
    , eval: eval bus
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }

render ∷ State → NotifyHTML
render st =
  HH.div
    [ HP.class_ (HH.className "sd-notifications") ]
    [ renderNotification Dismissed st.dismissed
    , renderNotification Current st.current
    ]

  where
  renderNotification status = maybe (HH.text "") \(dismiss × n) →
    HH.div
      [ HP.class_ (HH.className "sd-notification-spacer")
      , notificationKey status
      ]
      [ HH.div
          [ HP.classes (notificationClasses status n) ]
          [ HH.div
              [ HP.class_ (HH.className "sd-notification-text") ]
              [ HH.text (notificationText n) ]
          , HH.div
              [ HP.class_ (HH.className "sd-notification-buttons") ]
              [ HH.button
                  [ HP.class_ (HH.className "sd-notification-dismiss")
                  , HP.buttonType HP.ButtonButton
                  , HE.onClick (HE.input_ (Dismiss dismiss))
                  ]
                  [ HH.text "×" ]
              ]
          ]
      ]

  notificationKey = case _ of
    Current → HP.key $ "current-" ⊕ show st.tick
    Dismissed → HP.key $ "dismissed-" ⊕ show st.tick

  notificationClasses status n =
    [ HH.className "sd-notification"
    , HH.className case n of
        N.Info _    → "info"
        N.Warning _ → "warning"
        N.Error _   → "error"
    , HH.className case status of
        Current   → "current"
        Dismissed → "dismissed"
    ]

  notificationText = case _ of
    N.Info s → s
    N.Warning s → s
    N.Error s → s

eval
  ∷ ∀ r
  . Bus.Bus (read ∷ Bus.Cap | r) N.NotificationOptions
  → Query ~> NotifyDSL
eval bus = case _ of
  Init next →
    forever (raise <<< H.action <<< Push =<< H.fromAff (Bus.read bus))
  Push opts next → do
    dismiss ← H.fromAff makeVar
    H.modify \s → s { queue = Array.snoc s.queue (dismiss × opts) }
    current ← H.gets _.current
    when (isNothing current) drainQueue
    pure next
  Dismiss dismiss next → do
    H.fromAff $ putVar dismiss unit
    pure next

drainQueue ∷ NotifyDSL Unit
drainQueue = do
  st ← H.get
  case Array.uncons st.queue of
    Nothing → do
      H.modify _
        { tick = st.tick + 1
        , current = Nothing
        , dismissed = st.current
        }
    Just { head: dismiss × opts, tail } → do
      H.modify _
        { queue = tail
        , tick = st.tick + 1
        , current = Just (dismiss × opts.notification)
        , dismissed = st.current
        }

      for_ opts.timeout \(Milliseconds ms) →
        H.fromAff $ forkAff $ later' (Int.floor ms) (putVar dismiss unit)

      H.fromAff $ takeVar dismiss
      drainQueue
