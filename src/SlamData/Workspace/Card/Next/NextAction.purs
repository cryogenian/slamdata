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

module SlamData.Workspace.Card.Next.NextAction where

import SlamData.Prelude

import Data.Array as A

import SlamData.Workspace.Card.InsertableCardType (InsertableCardType)
import SlamData.Workspace.Card.InsertableCardType as ICT
import SlamData.Workspace.Card.CardType (CardType)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType (allChartTypes)
import SlamData.Workspace.Card.CardType.FormInputType (allFormInputTypes)
import SlamData.Workspace.Card.Port (Port)
import SlamData.ActionList.Component as ActionList

data NextAction
  = Insert CardType
  | FindOutHowToInsert CardType

chartSubmenu ∷ ActionList.Action NextAction
chartSubmenu =
  ActionList.Drill
    (ActionList.ActionName "Setup Chart")
    (ActionList.ActionIconSrc "img/cardsLight/setupChart.svg")
    (ActionList.ActionDescription "Select Setup Chart card category")
    (map (toAction ∘ Insert ∘ CT.ChartOptions) allChartTypes)

formInputSubmenu ∷ ActionList.Action NextAction
formInputSubmenu =
  ActionList.Drill
    (ActionList.ActionName "Setup Form")
    (ActionList.ActionIconSrc "img/cardsLight/setupFormInput.svg")
    (ActionList.ActionDescription "Select Setup Form card category")
    (map (toAction ∘ Insert ∘ CT.SetupFormInput) allFormInputTypes)


findOutHowToChartSubmenu ∷ ActionList.Action NextAction
findOutHowToChartSubmenu =
  ActionList.Drill
    (ActionList.ActionName "Setup Chart")
    (ActionList.ActionIconSrc "img/cardsLight/setupChart.svg")
    (ActionList.ActionDescription "Select Setup Chart card category")
    (map (toAction ∘ FindOutHowToInsert ∘ CT.ChartOptions) allChartTypes)

findOutHowToFormInputSubmenu ∷ ActionList.Action NextAction
findOutHowToFormInputSubmenu =
  ActionList.Drill
    (ActionList.ActionName "Setup Form")
    (ActionList.ActionIconSrc "img/cardsLight/setupFormInput.svg")
    (ActionList.ActionDescription "Select Setup Form card category")
    (map (toAction ∘ FindOutHowToInsert ∘ CT.SetupFormInput) allFormInputTypes)


toAction ∷ NextAction → ActionList.Action NextAction
toAction =
  case _ of
    nextAction@(Insert cardType) →
      ActionList.Do
        (name cardType)
        (ActionList.ActionIconSrc $ CT.cardIconLightSrc cardType)
        (description nextAction)
        (ActionList.ActionHighlighted true)
        (ActionList.ActionDisabled false)
        nextAction
    nextAction@(FindOutHowToInsert cardType) →
      ActionList.Do
        (name cardType)
        (ActionList.ActionIconSrc $ CT.cardIconLightSrc cardType)
        (description nextAction)
        (ActionList.ActionHighlighted false)
        (ActionList.ActionDisabled false)
        nextAction

name ∷ CardType → ActionList.ActionName
name =
  ActionList.ActionName ∘ CT.cardName

description ∷ NextAction → ActionList.ActionDescription
description =
  ActionList.ActionDescription ∘ case _ of
    Insert cty → "Insert a " ⊕ CT.cardName cty ⊕ " card"
    FindOutHowToInsert cty → "Find out how to insert a " ⊕ CT.cardName cty ⊕ " card"

derive instance eqNextAction ∷ Eq NextAction

insert ∷ InsertableCardType → ActionList.Action NextAction
insert =
  case _ of
    ICT.SetupChartCard → chartSubmenu
    ICT.SetupFormCard → formInputSubmenu
    iCardType → toAction $ Insert $ ICT.toCardType iCardType

findOutHowToInsert ∷ InsertableCardType → ActionList.Action NextAction
findOutHowToInsert =
  case _ of
    ICT.SetupChartCard → findOutHowToChartSubmenu
    ICT.SetupFormCard → findOutHowToFormInputSubmenu
    iCardType → toAction $ FindOutHowToInsert $ ICT.toCardType iCardType

fromInsertableCard ∷ InsertableCardType → Array InsertableCardType → ActionList.Action NextAction
fromInsertableCard x =
  maybe (findOutHowToInsert x) (const $ insert x) ∘ A.findIndex (eq x)

fromPort ∷ Port → Array (ActionList.Action NextAction)
fromPort port =
  flip fromInsertableCard (ICT.cardsThatTakeInput $ ICT.fromPort port) <$> ICT.all

fromMaybePort ∷ Maybe Port → Array (ActionList.Action NextAction)
fromMaybePort = maybe (flip fromInsertableCard (ICT.cardsThatTakeInput ICT.None) <$> ICT.all) fromPort

isInsert ∷ NextAction → Boolean
isInsert (Insert _) = true
isInsert _ = false
