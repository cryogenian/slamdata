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

import Halogen.HTML as H

import SlamData.Workspace.Card.InsertableCardType (InsertableCardType)
import SlamData.Workspace.Card.InsertableCardType as ICT
import SlamData.Workspace.Card.CardType (CardType, cardName, lightCardGlyph)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType (allChartTypes)
import SlamData.Workspace.Card.Port (Port)
import SlamData.ActionList.Component as ActionList

data NextAction
  = Insert CardType
  | FindOutHowToInsert CardType

chartSubmenu ∷ ActionList.Action NextAction
chartSubmenu =
  ActionList.Drill
    (ActionList.ActionName "Setup Chart")
    (ActionList.ActionIconUri "img/cardsLight/setupChart.svg")
    (ActionList.ActionDescription "Insert a Setup Chart card.")
    (map (toAction ∘ Insert ∘ CT.ChartOptions) allChartTypes)

findOutHowToChartSubmenu ∷ ActionList.Action NextAction
findOutHowToChartSubmenu =
  ActionList.Drill
    (ActionList.ActionName "Setup Chart")
    (ActionList.ActionIconUri "img/cardsLight/setupChart.svg")
    (ActionList.ActionDescription "Find out how to insert a Setup Chart card.")
    (map (toAction ∘ FindOutHowToInsert ∘ CT.ChartOptions) allChartTypes)

toAction ∷ NextAction → ActionList.Action NextAction
toAction =
  case _ of
    nextAction@(Insert cardType) →
      ActionList.Do
        (name cardType)
        (iconUri nextAction)
        (description nextAction)
        (ActionList.ActionHighlighted true)
        nextAction
    nextAction@(FindOutHowToInsert cardType) →
      ActionList.Do
        (name cardType)
        (iconUri nextAction)
        (description nextAction)
        (ActionList.ActionHighlighted false)
        nextAction

name ∷ CardType → ActionList.ActionName
name =
  ActionList.ActionName ∘ CT.cardName

description ∷ NextAction → ActionList.ActionDescription
description =
  ActionList.ActionDescription ∘ case _ of
    Insert cty → "Insert a " ⊕ CT.cardName cty ⊕ " card"
    FindOutHowToInsert cty → "Find out how to insert a " ⊕ CT.cardName cty ⊕ " card"

-- TODO: Icons
iconUri ∷ NextAction → ActionList.ActionIconUri
iconUri =
  const $ ActionList.ActionIconUri "http://placekitten.com/32/32"

instance eqNextAction ∷ Eq NextAction where
  eq (Insert cty1) (Insert cty2) = cty1 ≡ cty2
  eq (FindOutHowToInsert cty1) (FindOutHowToInsert cty2) = cty1 ≡ cty2
  eq _ _ = false

foldToArray ∷ NextAction → Array CardType
foldToArray = case _ of
  Insert cty → [ cty ]
  FindOutHowToInsert cty → [ cty ]

searchFilters ∷ NextAction → Array String
searchFilters (Insert cty) = [ cardName cty ]
searchFilters (FindOutHowToInsert cty) = [ cardName cty ]

label ∷ NextAction → String
label (Insert cty) = cardName cty
label (FindOutHowToInsert cty) = cardName cty

glyph ∷ ∀ s f. NextAction → H.HTML s f
glyph (Insert cty) = lightCardGlyph cty
glyph (FindOutHowToInsert cty) = lightCardGlyph cty

insert ∷ InsertableCardType → ActionList.Action NextAction
insert =
  case _ of
    ICT.SetupChartCard → chartSubmenu
    iCardType → toAction $ Insert $ ICT.toCardType iCardType

findOutHowToInsert ∷ InsertableCardType → ActionList.Action NextAction
findOutHowToInsert =
  case _ of
    ICT.SetupChartCard → findOutHowToChartSubmenu
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
