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

import Control.Monad.Eff.Exception as Exn
import Control.Monad.Error.Class as EC
import Control.Monad.Writer.Class as WC

import Data.Argonaut (decodeJson, encodeJson)
import Data.Lens ((.~))
import Data.Path.Pathy as Pt
import Data.StrMap as Sm

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B
import Halogen.HTML.Events.Indexed as HE

import SlamData.Effects (Slam)
import SlamData.Workspace.Card.CardType as Ct
import SlamData.Workspace.Card.Common.EvalQuery as Eq
import SlamData.Workspace.Card.Component as Cc
import SlamData.Workspace.Card.Port as P
import SlamData.Workspace.Card.Save.Component.Query (Query(..), QueryP)
import SlamData.Workspace.Card.Save.Component.State (State, initialState, _pathString)
import SlamData.Quasar.FS (messageIfFileNotFound) as Api
import SlamData.Quasar.Query (fileQuery) as Api
import SlamData.Render.CSS as Rc

import Utils.Path as Up

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
            , HP.value state.pathString
            , ARIA.label "Output file destination"
            , HE.onValueInput $ HE.input \s → right ∘ UpdatePathString s
            ]
        , HH.span
            [ HP.classes [ B.inputGroupBtn, Rc.saveCardButton ] ]
            [ HH.button
              [ HP.classes [ B.btn, B.btnPrimary ]
              , HP.buttonType HP.ButtonButton
              , ARIA.label "Confirm saving file"
              , HP.disabled $ isNothing $ Pt.parseAbsFile state.pathString
              , HE.onClick (HE.input_ $ left ∘ Cc.NotifyRunCard)
              ]
              [ HH.text "Save" ]
            ]
        ]
    ]

eval ∷ Natural QueryP SaveDSL
eval = coproduct cardEval saveEval

cardEval ∷ Natural Eq.CardEvalQuery SaveDSL
cardEval (Eq.EvalCard info k) =
  k <$> Eq.runCardEvalT do
    case info.inputPort of
      Just P.Blocked →
        pure Nothing
      Just (P.TaggedResource {tag, resource}) → do
        pt ← lift $ H.gets _.pathString
        case pt, Up.parseAnyPath pt of
          "", _ →
            pure Nothing
          _, Just (Right fp) → do
            outputResource ←
              Api.fileQuery resource fp "select * from {{path}}" Sm.empty
               # Eq.liftWithCanceler'
               # lift
               >>= either (EC.throwError <<< Exn.message) pure

            Api.messageIfFileNotFound
              outputResource
              "Error saving file, please try another location"
              # Eq.liftWithCanceler'
              # lift
              >>= either (EC.throwError <<< Exn.message) (traverse EC.throwError)

            when (fp ≠ outputResource)
              $ EC.throwError
              $ "Resource: " ⊕ Pt.printPath outputResource ⊕ " hasn't been modified"

            WC.tell ["Resource successfully saved as: " ⊕ Pt.printPath fp]

            pure ∘ Just $ P.TaggedResource { resource: outputResource, tag: Nothing }
          _, _ →
            EC.throwError $ pt ⊕ " is incorrect file path"

      _ →
        EC.throwError $ "Expected Resource input"
cardEval (Eq.NotifyRunCard next) = pure next
cardEval (Eq.NotifyStopCard next) = pure next
cardEval (Eq.Save k) = do
  pt ← H.gets _.pathString
  case Pt.parseAbsFile pt of
    Nothing → pure $ k $ encodeJson ""
    Just _ → pure $ k $ encodeJson pt
cardEval (Eq.Load js next) = do
  for_ (decodeJson js) \s → H.modify (_pathString .~ s)
  pure next
cardEval (Eq.SetupCard p next) = do
  H.modify (_pathString .~ (Pt.printPath $ Eq.temporaryOutputResource p))
  pure next
cardEval (Eq.SetCanceler _ next) = pure next
cardEval (Eq.SetDimensions _ cont) = pure $ cont true

saveEval ∷ Natural Query SaveDSL
saveEval (UpdatePathString str next) =
  H.modify (_pathString .~ str) $> next
