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

module SlamData.Workspace.Card.Save.Component
  ( saveCardComponent
  , module SlamData.Workspace.Card.Save.Component.State
  , module SlamData.Workspace.Card.Save.Component.Query
  ) where


import SlamData.Prelude

import Data.Lens ((.~), (?~))
import Data.Path.Pathy as Pt

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B
import Halogen.HTML.Events.Indexed as HE

import SlamData.Effects (Slam)
import SlamData.Render.CSS as Rc
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.CardType as Ct
import SlamData.Workspace.Card.Common.EvalQuery as Eq
import SlamData.Workspace.Card.Component as Cc
import SlamData.Workspace.Card.Save.Component.Query (Query(..), QueryP)
import SlamData.Workspace.Card.Save.Component.State (State, initialState, _pathString, _confirmedPath)

import Utils.Path as PU

type SaveHTML = H.ComponentHTML QueryP
type SaveDSL = H.ComponentDSL State QueryP Slam

saveCardComponent ∷ Cc.CardComponent
saveCardComponent = Cc.makeCardComponent
  { cardType: Ct.Save
  , component: H.component { render, eval }
  , initialState: initialState
  , _State: Cc._SaveState
  , _Query: Cc.makeQueryPrism Cc._SaveQuery
  }

render ∷ State → SaveHTML
render state =
  HH.div
    [ HP.classes
        [ Rc.exploreCardEditor
        , Rc.cardInput
        ]
    ]
    [
      HH.div [ HP.classes [ B.inputGroup, Rc.fileListField ] ]
        [ HH.input
            [ HP.classes [ B.formControl ]
            , HP.value $ fromMaybe "" state.pathString
            , ARIA.label "Output file destination"
            , HE.onValueInput $ HE.input \s → right ∘ UpdatePathString s
            ]
        , HH.span
            [ HP.classes [ B.inputGroupBtn, Rc.saveCardButton ] ]
            [ HH.button
              [ HP.classes [ B.btn, B.btnPrimary ]
              , HP.buttonType HP.ButtonButton
              , ARIA.label "Confirm saving file"
              , HP.disabled $ isNothing $ PU.parseFilePath =<< state.pathString
              , HE.onClick (HE.input_ $ right ∘ ConfirmPathString)
              ]
              [ HH.text "Save" ]
            ]
        ]
    ]

eval ∷ QueryP ~> SaveDSL
eval = coproduct cardEval saveEval

cardEval ∷ Eq.CardEvalQuery ~> SaveDSL
cardEval (Eq.EvalCard info output next) = do
  for_ output case _ of
    Port.TaggedResource { resource } →
      H.modify
        $ (_pathString ?~ Pt.printPath resource)
        ∘ (_confirmedPath ?~ resource)
    _ → pure unit
  pure next
cardEval (Eq.Save k) =
  k ∘ Card.Save ∘ map Pt.printPath <$> H.gets _.confirmedPath
cardEval (Eq.Load card next) = do
  case card of
    Card.Save s →
      H.modify
        $ (_pathString .~ s)
        ∘ (_confirmedPath .~ (PU.parseFilePath =<< s))
    _ → pure unit
  pure next
cardEval (Eq.SetDimensions _ next) = pure next

saveEval ∷ Query ~> SaveDSL
saveEval (UpdatePathString str next) =
  H.modify (_pathString ?~ str) $> next
saveEval (ConfirmPathString next) = do
  pathString ← H.gets _.pathString
  H.modify $ \st → st { confirmedPath = PU.parseFilePath =<< pathString }
  pure next
