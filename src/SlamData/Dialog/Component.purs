{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Dialog.Component
  ( DialogSpec
  , InnerComponent
  , InnerMessage(..)
  , Button
  , Buttons
  , Query(Raise, BackdropDismiss)
  , Message(..)
  , component
  ) where

import SlamData.Prelude

import Data.Array as A
import Data.List as L
import Data.List.NonEmpty as NEL
import DOM.Event.Types (MouseEvent)
import Halogen as H
import Halogen.Component.Proxy as Proxy
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN
import SlamData.Render.Common as RC
import Utils.DOM as DOM

type DialogSpec o =
  { title ∷ String
  , class_ ∷ H.ClassName
  , dialog ∷ InnerComponent o
  , buttons ∷ Buttons o
  }

type InnerComponent o = Proxy.ProxyComponent (Const Void) Unit (InnerMessage o) Slam

data InnerMessage o
  = SelfDismiss
  | Change (Buttons o)

type Button o =
  { label ∷ String
  , class_ ∷ H.ClassName
  , action ∷ Message o
  , disabled ∷ Boolean
  }

type Buttons o = NEL.NonEmptyList (Button o)

type State o =
  Maybe
    { pending ∷ Boolean
    , dialog ∷ DialogSpec o
    }

data Query i o a
  = ChangeDialog (Maybe i) a
  | HandleMessage (InnerMessage o) a
  | Raise (Message o) a
  | BackdropDismiss MouseEvent a

data Message o
  = Dismiss
  | Confirm o

type InnerQuery o = Proxy.ProxyQ (Const Void) Unit (InnerMessage o)
type HTML i o = H.ParentHTML (Query i o) (InnerQuery o) Unit Slam
type DSL i o = H.ParentDSL (State o) (Query i o) (InnerQuery o) Unit (Message o) Slam

component
  ∷ ∀ i o
  . (i → DialogSpec o)
  → H.Component HH.HTML (Query i o) (Maybe i) (Message o) Slam
component mkInner =
  H.parentComponent
    { render
    , eval: eval mkInner
    , initialState: const Nothing
    , receiver: HE.input ChangeDialog
    }

render ∷ ∀ i o. State o → HTML i o
render = case _ of
  Nothing → HH.text ""
  Just { pending, dialog: { title, class_, dialog, buttons } } →
    HH.div
      [ HP.class_ (H.ClassName "sd-dialog-container")
      , HE.onClick $ HE.input BackdropDismiss
      ]
      [ HH.div
          [ HP.classes [ H.ClassName "sd-dialog", class_ ] ]
          [ HH.div
              [ HP.class_ (HH.ClassName "sd-dialog-header") ]
              $ join
                [ pure $
                    HH.div
                      [ HP.class_ (HH.ClassName "sd-dialog-title") ]
                      [ HH.h4_ [ HH.text title ] ]
                , guard pending $> RC.spinner
                ]
          , HH.div
              [ HP.class_ (H.ClassName "sd-dialog-body") ]
              [ HH.slot unit dialog unit (HE.input HandleMessage) ]
          , HH.div
              [ HP.class_ (H.ClassName "sd-dialog-footer") ]
              (renderButtons pending buttons)
          ]
      ]

renderButtons ∷ ∀ i o. Boolean → Buttons o → Array (HTML i o)
renderButtons pending = go <<< L.reverse <<< L.fromFoldable
  where
  go xs =
    let
      lefts = A.fromFoldable (L.drop 2 xs)
      rights = A.fromFoldable (L.take 2 xs)
    in
      join
        [ guard (not A.null lefts) $>
            HH.div
              [ HP.class_ (H.ClassName "sd-dialog-footer-left") ]
              (renderButton pending <$> lefts)
        , pure $
            HH.div
              [ HP.class_ (H.ClassName "sd-dialog-footer-right") ]
              (renderButton pending <$> rights)
        ]

renderButton ∷ ∀ i o. Boolean → Button o → HTML i o
renderButton pending { label, class_, action, disabled } =
  HH.button
    [ HP.classes [ CN.btn, class_ ]
    , HP.type_ HP.ButtonButton
    , HP.disabled (pending || disabled)
    , HE.onClick (HE.input_ (Raise action))
    ]
    [ HH.text label ]

eval ∷ ∀ i o. (i → DialogSpec o) → Query i o ~> DSL i o
eval mkInner = case _ of
  ChangeDialog i next → do
    H.put $ { pending: false, dialog: _ } ∘ mkInner <$> i
    pure next
  HandleMessage msg next → do
    case msg of
      SelfDismiss →
        H.raise Dismiss
      Change buttons →
        H.modify (map (_ { dialog { buttons = buttons } }))
    pure next
  Raise msg next → do
    H.modify (map (_ { pending = true }))
    H.raise msg
    pure next
  BackdropDismiss me next → do
    isDialog ← H.liftEff $ DOM.nodeEq (DOM.target me) (DOM.currentTarget me)
    when isDialog (H.raise Dismiss)
    pure next
