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

module SlamData.Workspace.Card.Next.Component
 ( nextCardComponent
 , module SlamData.Workspace.Card.Next.Component.Query
 , module SlamData.Workspace.Card.Next.Component.State
 , module NA
 ) where

import SlamData.Prelude

import CSS as CSS

import Data.Lens ((.~))

import Halogen as H
import Halogen.Component.ChildPath (cpI)
import Halogen.Component.Utils as HU
import Halogen.HTML.CSS.Indexed as HCSS
import Halogen.HTML.Indexed as HH

import SlamData.ActionList.Component as ActionList
import SlamData.Monad (Slam)
import SlamData.Guide as Guide
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.InsertableCardType as ICT
import SlamData.Workspace.Card.Next.NextAction as NA
import SlamData.Workspace.Card.Next.Component.Query (QueryP, Query(..), _AddCardType, _PresentReason)
import SlamData.Workspace.Card.Next.Component.State (StateP, State, initialState)
import SlamData.Workspace.Card.Next.Component.State as State
import SlamData.Workspace.Card.Port as Port

import Utils.LocalStorage as LocalStorage

type NextHTML =
  H.ParentHTML
    (ActionList.State NA.NextAction)
    Query
    (ActionList.Query NA.NextAction)
    Slam
    Unit

type NextDSL =
  H.ParentDSL
    State
    (ActionList.State NA.NextAction)
    Query
    (ActionList.Query NA.NextAction)
    Slam
    Unit

nextCardComponent ∷ H.Component StateP QueryP Slam
nextCardComponent = H.parentComponent
  { render
  , eval
  , peek: Just (peek ∘ H.runChildF)
  }

render ∷ State → NextHTML
render state =
  HH.div
    [ HCSS.style $ CSS.width (CSS.pct 100.0) *> CSS.height (CSS.pct 100.0) ]
    $ (guard state.presentAddCardGuide $>
        Guide.render
          Guide.DownArrow
          (HH.className "sd-add-card-guide")
          (DismissAddCardGuide)
          (addCardGuideText state.input))
    ⊕ [ HH.slot' cpI unit \_ →
        { component:
            ActionList.comp
              (ActionList.FilterInputDescription "Filter next actions")
        , initialState:
            ActionList.initialState
              (NA.fromPort state.input)
        }
      ]
  where
  addCardGuideText = case _ of
    Port.Initial → "To get this deck started press one of these buttons to add a card."
    _            → "To do more with this deck press one of these buttons to add a card."

updateActions ∷ Port.Port → NextDSL Unit
updateActions =
  void
    ∘ H.query' cpI unit
    ∘ H.action
    ∘ ActionList.UpdateActions
    ∘ NA.fromPort

takesInput ∷ Port.Port → CT.CardType → Boolean
takesInput input =
  ICT.takesInput (ICT.fromPort input) ∘ ICT.fromCardType

possibleToGetTo ∷ Port.Port → CT.CardType → Boolean
possibleToGetTo input =
  ICT.possibleToGetTo (ICT.fromPort input) ∘ ICT.fromCardType

dismissedAddCardGuideKey ∷ String
dismissedAddCardGuideKey = "dismissedAddCardGuide"

getDismissedAddCardGuideBefore ∷ NextDSL Boolean
getDismissedAddCardGuideBefore =
  H.liftH $ H.liftH $ either (const $ false) id <$>
    LocalStorage.getLocalStorage dismissedAddCardGuideKey

storeDismissedAddCardGuide ∷ NextDSL Unit
storeDismissedAddCardGuide =
  H.liftH $ H.liftH $ LocalStorage.setLocalStorage dismissedAddCardGuideKey true

dismissAddCardGuide ∷ NextDSL Unit
dismissAddCardGuide =
  H.modify (State._presentAddCardGuide .~ false)
    *> storeDismissedAddCardGuide

eval ∷ Query ~> NextDSL
eval = case _ of
  UpdateInput input next → updateActions input $> next
  AddCard _ next → dismissAddCardGuide $> next
  PresentReason input cardType next → pure next
  DismissAddCardGuide next → dismissAddCardGuide $> next
  PresentAddCardGuide next →
    (H.modify
       ∘ (State._presentAddCardGuide .~ _)
       ∘ not =<< getDismissedAddCardGuideBefore)
       $> next

peek ∷ ∀ a. ActionList.Query NA.NextAction a → NextDSL Unit
peek = case _ of
  ActionList.Selected action _ →
    case action of
      ActionList.GoBackInternal →
        pure unit
      ActionList.DrillInternal _ _ _ _ →
        pure unit
      ActionList.DoInternal _ _ _ _ nextAction →
        case nextAction of
          NA.Insert cardType →
            HU.raise' $ H.action $ AddCard cardType
          NA.FindOutHowToInsert cardType → do
            input ← H.gets _.input
            HU.raise' $ H.action $ PresentReason input cardType
  _ → pure unit
