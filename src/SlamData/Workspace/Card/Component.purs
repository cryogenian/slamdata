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

import Control.Coroutine.Stalling (producerToStallingProducer)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Control.Monad.Free (liftF)
import Control.Monad.Aff (cancel)
import Control.Monad.Eff.Exception as Exn

import Data.Array as Arr
import Data.Argonaut (jsonNull)
import Data.Date as Date
import Data.Function (on)
import Data.Lens (PrismP, review, preview, clonePrism, (.~), (%~))
import Data.Visibility (Visibility(..), toggleVisibility)

import DOM.Timer (interval, clearInterval)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Query.EventSource (EventSource(..))
import Halogen.Query.HalogenF (HalogenFP(..))
import SlamData.Effects (Slam)
import SlamData.Workspace.Card.CardType (cardClasses, nextCardTypes)
import SlamData.Workspace.Card.Component.Def (CardDef, makeQueryPrism, makeQueryPrism')

import SlamData.Workspace.Card.Component.Query as CQ
import SlamData.Workspace.Card.Component.Render as CR
import SlamData.Workspace.Card.Component.State as CS
import SlamData.Workspace.Card.Port (_Blocked)
import SlamData.Workspace.Card.RunState (RunState(..))
import SlamData.Render.CSS as CSS

import Utils.AffableProducer (produce)

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
    if cs.visibility ≡ Invisible
      then HH.text ""
      else shown cs
    where
    shown cs =
      HH.div
        [ HP.classes $ [ CSS.deckCard ] <> collapsedClass ]
        $ fold
          [ CR.header def.cardType cs
          , [ HH.div
                [ hideIfCollapsed
                , HP.classes $ cardClasses def.cardType
                ]
                [ HH.slot unit \_ → {component, initialState} ]
            ]
          , (guard canHaveOutput) $> CR.statusBar cs.hasResults cs
          ]
    canHaveOutput = not $ Arr.null $ nextCardTypes $ Just def.cardType
    shouldCollapse = cs.isCollapsed || isJust (cs.input >>= preview _Blocked)
    collapsedClass = guard shouldCollapse $> CSS.collapsed
    hideIfCollapsed = ARIA.hidden $ show shouldCollapse

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
  H.parentComponent
    { render: render component initialState
    , eval
    , peek: Just (peek ∘ H.runChildF)
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
  eval (CQ.RunCard next) = pure next
  eval (CQ.StopCard next) = stopRun $> next
  eval (CQ.UpdateCard input k) = do
    H.fromAff =<< H.gets _.tickStopper
    tickStopper ← startInterval
    H.modify
      $ (CS._tickStopper .~ tickStopper)
      ∘ (CS._input .~ input.inputPort)
    result ← H.query unit (left (H.request (CQ.EvalCard input)))
    for_ result \{ output } → H.modify (CS._hasResults .~ isJust output)
    H.fromAff tickStopper
    H.modify
      $ (CS._runState %~ finishRun)
      ∘ (CS._output .~ (_.output =<< result))
      ∘ (CS._messages .~ (maybe [] _.messages result))
    maybe (liftF HaltHF) (pure ∘ k ∘ _.output) result
  eval (CQ.RefreshCard next) = pure next
  eval (CQ.TrashCard next) = pure next
  eval (CQ.ToggleCollapsed next) =
    H.modify (CS._isCollapsed %~ not) $> next
  eval (CQ.ToggleMessages next) =
    H.modify (CS._messageVisibility %~ toggleVisibility) $> next
  eval (CQ.Tick elapsed next) =
    H.modify (CS._runState .~ RunElapsed elapsed) $> next
  eval (CQ.GetOutput k) = k <$> H.gets (_.output)
  eval (CQ.SaveCard cardId cardType k) = do
    { hasResults } ← H.get
    json ← H.query unit (left (H.request CQ.Save))
    pure ∘ k $
      { cardId
      , cardType
      , hasRun: hasResults
      , state: fromMaybe jsonNull json
      }
  eval (CQ.LoadCard model next) = do
    H.query unit (left (H.action (CQ.Load model.state)))
    pure next
  eval (CQ.SetCardAccessType at next) =
    H.modify (CS._accessType .~ at) $> next

  peek ∷ ∀ a. CQ.InnerCardQuery a → CardDSL Unit
  peek = coproduct cardEvalPeek (const $ pure unit)

  cardEvalPeek ∷ ∀ a. CQ.CardEvalQuery a → CardDSL Unit
  cardEvalPeek (CQ.SetCanceler canceler _) = H.modify $ CS._canceler .~ canceler
  cardEvalPeek (CQ.SetupCard _ _) = H.modify $ CS._canceler .~ mempty
  cardEvalPeek (CQ.EvalCard _ _) = H.modify $ CS._canceler .~ mempty
  cardEvalPeek (CQ.NotifyStopCard _) = stopRun
  cardEvalPeek _ = pure unit

  stopRun ∷ CardDSL Unit
  stopRun = do
    cs ← H.gets _.canceler
    ts ← H.gets _.tickStopper
    H.fromAff ts
    H.fromAff $ cancel cs (Exn.error "Canceled")
    H.modify $ CS._runState .~ RunInitial

-- | Starts a timer running on an interval that passes Tick queries back to the
-- | component, allowing the runState to be updated with a timer.
-- |
-- | The returned value is an action that will stop the timer running when
-- | processed.
startInterval ∷ CardDSL (Slam Unit)
startInterval = do
  ref ← H.fromEff (newRef Nothing)
  start ← H.fromEff Date.now
  H.modify (CS._runState .~ RunElapsed zero)

  H.subscribe'
    $ EventSource
    $ producerToStallingProducer
    $ produce \emit → do
        i ← interval 1000 $ emit ∘ Left ∘ H.action ∘ CQ.Tick =<< do
          now ← Date.now
          pure $ on (-) Date.toEpochMilliseconds now start
        writeRef ref (Just i)

  pure $ maybe (pure unit) (H.fromEff ∘ clearInterval) =<< H.fromEff (readRef ref)

-- | Update the `RunState` from its current value to `RunFinished`.
finishRun ∷ RunState → RunState
finishRun RunInitial = RunElapsed zero
finishRun (RunElapsed ms) = RunFinished ms
finishRun (RunFinished ms) = RunFinished ms
