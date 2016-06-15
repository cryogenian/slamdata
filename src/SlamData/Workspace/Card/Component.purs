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

module SlamData.Workspace.Card.Component
  ( CardComponent
  , makeCardComponent
  , module SlamData.Workspace.Card.Component.Def
  , module CQ
  , module CS
  ) where

import SlamData.Prelude
import SlamData.Config as Config

import Data.Time (Milliseconds(..))
import Data.Lens (PrismP, (.~), review, preview, clonePrism)

import Halogen as H
import Halogen.Component.Utils (sendAfter')
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import Math as Math

import SlamData.Effects (Slam)
import SlamData.Workspace.Card.CardType (cardClasses)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Component.Def (CardDef, makeQueryPrism, makeQueryPrism')
import SlamData.Workspace.Card.Component.Query as CQ
import SlamData.Workspace.Card.Component.Render as CR
import SlamData.Workspace.Card.Component.State as CS
import SlamData.Render.CSS as CSS

import Utils.DOM as DOMUtils

-- | Type synonym for the full type of a card component.
type CardComponent = H.Component CS.CardStateP CQ.CardQueryP Slam
type CardDSL = H.ParentDSL CS.CardState CS.AnyCardState CQ.CardQuery CQ.InnerCardQuery Slam Unit

-- | Card component factory
makeCardComponent
  ∷ ∀ s f
  . CardDef s f ()
  → CardComponent
makeCardComponent def = makeCardComponentPart def render
  where
  render
    ∷ H.Component CS.AnyCardState CQ.InnerCardQuery Slam
    → CS.AnyCardState
    → CS.CardState
    → CR.CardHTML
  render component initialState cs =
    HH.div
      [ HP.classes $ [ CSS.deckCard ]
      , HP.ref (H.action ∘ CQ.SetHTMLElement)
      ]
      $ fold
        [ CR.header def.cardType cs
        , [ HH.div
              [ HP.classes $ cardClasses def.cardType ]
              [ HH.slot unit \_ → {component, initialState} ]
          ]
        ]

-- | Constructs a card component from a record with the necessary properties and
-- | a render function.
makeCardComponentPart
  ∷ ∀ s f r
  . CardDef s f r
  → (H.Component CS.AnyCardState CQ.InnerCardQuery Slam
     → CS.AnyCardState
     → CS.CardState
     → CR.CardHTML)
  → CardComponent
makeCardComponentPart def render =
  H.lifecycleParentComponent
    { render: render component initialState
    , eval
    , peek: Nothing
    , initializer: Just (H.action CQ.UpdateDimensions)
    , finalizer: Nothing
    }
  where

  _State ∷ PrismP CS.AnyCardState s
  _State = clonePrism def._State

  _Query ∷ ∀ a. PrismP (CQ.InnerCardQuery a) (f a)
  _Query = clonePrism def._Query

  component ∷ H.Component CS.AnyCardState CQ.InnerCardQuery Slam
  component =
    H.transform
      (review _State) (preview _State)
      (review _Query) (preview _Query)
      def.component

  initialState ∷ CS.AnyCardState
  initialState = review _State def.initialState

  eval ∷ Natural CQ.CardQuery CardDSL
  eval (CQ.UpdateCard input output next) = do
    void $ H.query unit (left (H.action (CQ.EvalCard input output)))
    H.modify $ CS._output .~ output
    pure next
  eval (CQ.SaveCard cardId cardType k) = do
    model ← fromMaybe (Card.cardModelOfType cardType) <$> H.query unit (left (H.request CQ.Save))
    pure $ k { cardId, model }
  eval (CQ.LoadCard card next) = do
    H.query unit ∘ left ∘ H.action $ CQ.Load card.model
    sendAfter' (Milliseconds 100.0) (CQ.UpdateDimensions unit)
    pure next
  eval (CQ.SetCardAccessType at next) =
    H.modify (CS._accessType .~ at) $> next
  eval (CQ.SetHTMLElement el next) =
    H.modify (CS._element .~ el) $> next
  eval (CQ.UpdateDimensions next) = do
    H.gets _.element >>= traverse_ \el -> do
      { width, height } ← H.fromEff (DOMUtils.getBoundingClientRect el)
      let
        round n = (Math.round (n / Config.gridPx)) * Config.gridPx
        roundedWidth = round width
        roundedHeight = round height
      unless (roundedWidth ≡ zero ∧ roundedHeight ≡ zero)
        $ void
        $ H.query unit
        $ left
        $ H.action
        $ CQ.SetDimensions
            { width: roundedWidth
            , height: roundedHeight
            }
    pure next
