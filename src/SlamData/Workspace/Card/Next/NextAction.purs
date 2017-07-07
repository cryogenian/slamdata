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

import SlamData.ActionList.Action as Action
import SlamData.Workspace.Card.CardType (CardType)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.Chart as Cht
import SlamData.Workspace.Card.CardType.Select as Sel
import SlamData.Workspace.Card.CardType.Input as Inp
import SlamData.Workspace.Card.CardType.Static as Sta
import SlamData.Workspace.Card.CardType.Geo as Geo
import SlamData.Workspace.Card.InsertableCardType (InsertableCardType)
import SlamData.Workspace.Card.InsertableCardType as ICT
import SlamData.Workspace.Card.Port (Port)

data NextAction
  = Insert CardType
  | FindOutHowToInsert CardType


instance eqNextAction ∷ Eq NextAction where
  eq (Insert c1) (Insert c2) = CT.eq_ c1 c2
  eq (FindOutHowToInsert c1) (FindOutHowToInsert c2) = CT.eq_ c1 c2
  eq _ _ = false

--derive instance eqNextAction ∷ Eq NextAction

isInsert ∷ NextAction → Boolean
isInsert (Insert _) = true
isInsert _ = false

pluckCardType ∷ NextAction → CardType
pluckCardType (Insert ct) = ct
pluckCardType (FindOutHowToInsert ct) = ct

geoChartSubmenu
  ∷ ∀ r
  . (CT.Geo r → Action.Action NextAction)
  → Action.Action NextAction
geoChartSubmenu mkAction =
  Action.mkDrill
    { name: "Setup Geo Chart"
    , iconSrc: "img/cardsLight/setupGeoChart.svg"
    , description: "Select Setup Geo Chart card category"
    , children: map mkAction Geo.all
    }

insertGeoChartSubmenu ∷ Action.Action NextAction
insertGeoChartSubmenu =
  geoChartSubmenu $ toAction ∘ Insert

findOutHowToGeoChartSubmenu ∷ Action.Action NextAction
findOutHowToGeoChartSubmenu =
  geoChartSubmenu $ toAction ∘ FindOutHowToInsert

chartSubmenu
  ∷ ∀ r
  . (CT.Chart r → Action.Action NextAction)
  → Action.Action NextAction
chartSubmenu mkAction =
  Action.mkDrill
    { name: "Setup Chart"
    , iconSrc: "img/cardsLight/setupChart.svg"
    , description: "Select Setup Chart card category"
    , children: map mkAction Cht.all
    }

insertChartSubmenu ∷ Action.Action NextAction
insertChartSubmenu =
  chartSubmenu $ toAction ∘ Insert

findOutHowToChartSubmenu ∷ Action.Action NextAction
findOutHowToChartSubmenu =
  chartSubmenu $ toAction ∘ FindOutHowToInsert

formInputSubmenu
  ∷ ∀ r
  . (Variant (CT.StaticR (CT.SelectR (CT.InputR r))) → Action.Action NextAction)
  → Action.Action NextAction
formInputSubmenu mkAction =
  Action.mkDrill
    { name: "Setup Form"
    , iconSrc: "img/cardsLight/setupFormInput.svg"
    , description: "Select Setup Form card category"
    , children: map mkAction $ Sta.all ⊕ Inp.all ⊕ Sel.all
    }

insertFormInputSubmenu ∷ Action.Action NextAction
insertFormInputSubmenu =
  formInputSubmenu $ toAction ∘ Insert

findOutHowToFormInputSubmenu ∷ Action.Action NextAction
findOutHowToFormInputSubmenu =
  formInputSubmenu $ toAction ∘ FindOutHowToInsert

toAction ∷ NextAction → Action.Action NextAction
toAction na =
  Action.mkDo
    { name: CT.name cardType
    , iconSrc: CT.lightIconSrc cardType
    , highlighted: isInsert na
    , disabled: false
    , description: description na
    , action: na
    }
  where
  cardType = pluckCardType na

description ∷ NextAction → String
description = case _ of
  Insert cty → "Insert a " ⊕ CT.name cty ⊕ " card"
  FindOutHowToInsert cty → "Find out how to insert a " ⊕ CT.name cty ⊕ " card"

insert ∷ InsertableCardType → Array (Action.Action NextAction)
insert = case _ of
  ICT.SetupChartCard → A.singleton insertChartSubmenu
  ICT.SetupFormCard → A.singleton insertFormInputSubmenu
  ICT.SetupGeoChartCard → A.singleton insertGeoChartSubmenu
  iCardType → foldMap (A.singleton ∘ toAction ∘ Insert) $ ICT.toCardType iCardType

findOutHowToInsert ∷ InsertableCardType → Array (Action.Action NextAction)
findOutHowToInsert = case _ of
  ICT.SetupChartCard → A.singleton findOutHowToChartSubmenu
  ICT.SetupFormCard → A.singleton findOutHowToFormInputSubmenu
  ICT.SetupGeoChartCard → A.singleton findOutHowToGeoChartSubmenu
  iCardType → foldMap (A.singleton ∘ toAction ∘ FindOutHowToInsert) $ ICT.toCardType iCardType

fromInsertableCard
  ∷ InsertableCardType
  → Array InsertableCardType
  → Array (Action.Action NextAction)
fromInsertableCard icard allCards = case A.elemIndex icard allCards of
  Nothing → findOutHowToInsert icard
  Just _ → insert icard

fromPort ∷ Port → Array (Action.Action NextAction)
fromPort port = do
  sample ← ICT.all
  fromInsertableCard sample cardsTakingInputFromPort
  where
  ioCard = ICT.fromPort port
  cardsTakingInputFromPort = ICT.cardsThatTakeInput ioCard
