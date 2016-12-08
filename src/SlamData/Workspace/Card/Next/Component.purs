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
 , module SlamData.Workspace.Card.Next.Component.State
 , module SlamData.Workspace.Card.Next.Component.Query
 ) where

import SlamData.Prelude

import Data.Foldable as F
import Data.Lens ((.~))

import Halogen as H
import Halogen.Component.ChildPath (cpI)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B
import Halogen.Component.Utils as HU

import SlamData.ActionList.Component as ActionList
import SlamData.Monad (Slam)
import SlamData.Guide as Guide
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Next.NextAction as NA
import SlamData.Workspace.Card.Next.Component.Query (QueryC, QueryP, Query(..), _AddCardType, _PresentReason)
import SlamData.Workspace.Card.Next.Component.State (StateP, State)
import SlamData.Workspace.Card.Next.Component.State as State
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.InsertableCardType as ICT

import Utils.LocalStorage as LocalStorage

type NextHTML =
  H.ParentHTML
    (ActionList.State NA.NextAction)
    QueryC
    (ActionList.Query NA.NextAction)
    Slam
    Unit

type NextDSL =
  H.ParentDSL
    State
    (ActionList.State NA.NextAction)
    QueryC
    (ActionList.Query NA.NextAction)
    Slam
    Unit

nextCardComponent ∷ CC.CardComponent
nextCardComponent = CC.makeCardComponent
  { cardType: CT.NextAction
  , component:
      H.parentComponent
        { render: render
        , peek: Just (peek ∘ H.runChildF)
        , eval: eval
        }
  , initialState: H.parentState State.initialState
  , _State: CC._NextState
  , _Query: CC.makeQueryPrism' CC._NextQuery
  }

render ∷ State → NextHTML
render state =
  HH.div_
    $ (guard (not state.presentAddCardGuide) $>
      HH.form_
        [ HH.div_
            [ HH.slot' cpI unit \_ →
                { component: ActionList.comp
                , initialState: ActionList.initialState
                }
            ]
        ])
    ⊕ (guard state.presentAddCardGuide $>
        Guide.render
          Guide.DownArrow
          (HH.className "sd-add-card-guide")
          (right ∘ DismissAddCardGuide)
          (addCardGuideText state.input))
  where
  addCardGuideTextEmptyDeck = "To get this deck started press one of these buttons to add a card."
  addCardGuideTextNonEmptyDeck = "To do more with this deck press one of these buttons to add a card."
  addCardGuideText = maybe addCardGuideTextEmptyDeck (const addCardGuideTextNonEmptyDeck)

eval ∷ QueryC ~> NextDSL
eval = coproduct cardEval nextEval

cardEval ∷ CC.CardEvalQuery ~> NextDSL
cardEval = case _ of
  CC.EvalCard value output next → do
    H.modify $ State._input .~ value.input
    updateActions value.input
    pure next
  CC.Activate next →
    (updateActions =<< H.gets _.input) $> next
  CC.Deactivate next →
    pure next
  CC.Save k →
    pure $ k Card.NextAction
  CC.Load _ next →
    pure next
  CC.SetDimensions _ next →
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

updateActions ∷ Maybe Port.Port → NextDSL Unit
updateActions =
  void
    ∘ H.query' cpI unit
    ∘ H.action
    ∘ ActionList.UpdateActions
    ∘ NA.fromMaybePort

takesInput ∷ Maybe Port.Port → CT.CardType → Boolean
takesInput input =
  maybe false (ICT.takesInput $ ICT.fromMaybePort input) ∘ ICT.fromCardType

possibleToGetTo ∷ Maybe Port.Port → CT.CardType → Boolean
possibleToGetTo input =
  maybe false (ICT.possibleToGetTo $ ICT.fromMaybePort input) ∘ ICT.fromCardType

dismissedAddCardGuideKey ∷ String
dismissedAddCardGuideKey = "dismissedAddCardGuide"

getDismissedAddCardGuideBefore ∷ NextDSL Boolean
getDismissedAddCardGuideBefore =
  H.liftH
    $ H.liftH
    $ either (const $ false) id
    <$> LocalStorage.getLocalStorage dismissedAddCardGuideKey

storeDismissedAddCardGuide ∷ NextDSL Unit
storeDismissedAddCardGuide =
  H.liftH $ H.liftH $ LocalStorage.setLocalStorage dismissedAddCardGuideKey true

dismissAddCardGuide ∷ NextDSL Unit
dismissAddCardGuide =
  H.modify (State._presentAddCardGuide .~ false)
    *> storeDismissedAddCardGuide

nextEval ∷ Query ~> NextDSL
nextEval (AddCard _ next) = dismissAddCardGuide $> next
nextEval (PresentReason input cardType next) = pure next
nextEval (DismissAddCardGuide next) = dismissAddCardGuide $> next
nextEval (PresentAddCardGuide next) =
  (H.modify
     ∘ (State._presentAddCardGuide .~ _)
     ∘ not =<< getDismissedAddCardGuideBefore)
     $> next

peek ∷ ∀ a. ActionList.Query NA.NextAction a → NextDSL Unit
peek = case _ of
  ActionList.Selected action _ →
    case action of
      ActionList.GoBack →
        pure unit
      ActionList.Drill _ _ _ _ →
        pure unit
      ActionList.Do _ _ _ _ nextAction →
        case nextAction of
          NA.Insert cardType →
            HU.raise' $ right $ H.action $ AddCard cardType
          NA.FindOutHowToInsert cardType → do
            input ← H.gets _.input
            HU.raise' $ right $ H.action $ PresentReason input cardType
  _ → pure unit
