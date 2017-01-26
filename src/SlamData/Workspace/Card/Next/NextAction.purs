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
import SlamData.Workspace.Card.CardType.ChartType (ChartType, allChartTypes)
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType, allFormInputTypes)
import SlamData.Workspace.Card.Port (Port)
import SlamData.ActionList.Action as Action

data NextAction
  = Insert CardType
  | FindOutHowToInsert CardType

derive instance eqNextAction ∷ Eq NextAction

isInsert ∷ NextAction → Boolean
isInsert (Insert _) = true
isInsert _ = false

pluckCardType ∷ NextAction → CardType
pluckCardType (Insert ct) = ct
pluckCardType (FindOutHowToInsert ct) = ct

chartSubmenu
  ∷ (ChartType → Action.Action NextAction)
  → Action.Action NextAction
chartSubmenu mkAction =
  Action.mkDrill
    { name: "Setup Chart"
    , iconSrc: "img/cardsLight/setupChart.svg"
    , description: "Select Setup Chart card category"
    , children: map mkAction allChartTypes
    }

insertChartSubmenu ∷ Action.Action NextAction
insertChartSubmenu =
  chartSubmenu $ toAction ∘ Insert ∘ CT.ChartOptions

findOutHowToChartSubmenu ∷ Action.Action NextAction
findOutHowToChartSubmenu =
  chartSubmenu $ toAction ∘ FindOutHowToInsert ∘ CT.ChartOptions

formInputSubmenu
  ∷ (FormInputType → Action.Action NextAction)
  → Action.Action NextAction
formInputSubmenu mkAction =
  Action.mkDrill
    { name: "Setup Form"
    , iconSrc: "img/cardsLight/setupFormInput.svg"
    , description: "Select Setup Form card category"
    , children: map mkAction allFormInputTypes
    }

insertFormInputSubmenu ∷ Action.Action NextAction
insertFormInputSubmenu =
  formInputSubmenu $ toAction ∘ Insert ∘ CT.SetupFormInput

findOutHowToFormInputSubmenu ∷ Action.Action NextAction
findOutHowToFormInputSubmenu =
  formInputSubmenu $ toAction ∘ FindOutHowToInsert ∘ CT.SetupFormInput

toAction ∷ NextAction → Action.Action NextAction
toAction na =
  Action.mkDo
    { name: CT.cardName cardType
    , iconSrc: CT.cardIconLightSrc cardType
    , highlighted: isInsert na
    , disabled: false
    , description: description na
    , action: na
    }
  where
  cardType = pluckCardType na

description ∷ NextAction → String
description = case _ of
  Insert cty → "Insert a " ⊕ CT.cardName cty ⊕ " card"
  FindOutHowToInsert cty → "Find out how to insert a " ⊕ CT.cardName cty ⊕ " card"

insert ∷ InsertableCardType → Action.Action NextAction
insert = case _ of
  ICT.SetupChartCard → insertChartSubmenu
  ICT.SetupFormCard → insertFormInputSubmenu
  iCardType → toAction $ Insert $ ICT.toCardType iCardType

findOutHowToInsert ∷ InsertableCardType → Action.Action NextAction
findOutHowToInsert = case _ of
  ICT.SetupChartCard → findOutHowToChartSubmenu
  ICT.SetupFormCard → findOutHowToFormInputSubmenu
  iCardType → toAction $ FindOutHowToInsert $ ICT.toCardType iCardType

fromInsertableCard
  ∷ InsertableCardType
  → Array InsertableCardType
  → Action.Action NextAction
fromInsertableCard x =
  maybe (findOutHowToInsert x) (const $ insert x) ∘ A.findIndex (eq x)

fromPort ∷ Port → Array (Action.Action NextAction)
fromPort port =
  flip fromInsertableCard (ICT.cardsThatTakeInput $ ICT.fromPort port) <$> ICT.all

fromMaybePort ∷ Maybe Port → Array (Action.Action NextAction)
fromMaybePort = maybe (flip fromInsertableCard (ICT.cardsThatTakeInput ICT.None) <$> ICT.all) fromPort
