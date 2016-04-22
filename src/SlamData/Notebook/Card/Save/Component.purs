module SlamData.Notebook.Card.Save.Component
  ( saveCardComponent
  , module SlamData.Notebook.Card.Save.Component.State
  , module SlamData.Notebook.Card.Save.Component.Query
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
import SlamData.Notebook.Card.CardType as Ct
import SlamData.Notebook.Card.Common.EvalQuery as Eq
import SlamData.Notebook.Card.Component as Cc
import SlamData.Notebook.Card.Port as P
import SlamData.Notebook.Card.Save.Component.Query (Query(..), QueryP)
import SlamData.Notebook.Card.Save.Component.State (State, initialState, _pathString)
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
cardEval (Eq.EvalCard info k) = case info.inputPort of
  Just P.Blocked →
    pure $ k { output: Nothing, messages: [ ] }
  Just (P.TaggedResource {tag, resource}) → do
    pt ← H.gets _.pathString
    case pt, Up.parseAnyPath pt of
      "", _ →
        pure $ k { output: Nothing, messages: [ ] }
      _, Just (Right fp) → map k $ Eq.runCardEvalT do

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

        pure $ P.TaggedResource { resource: outputResource, tag: Nothing }
      _, _ →
        pure $ k { output: Just P.Blocked
                 , messages: [ Left $ pt ⊕ " is incorrect file path" ] }

  _ →
    pure $ k { output: Nothing, messages: [ Left "Expected Resource input" ] }
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

saveEval ∷ Natural Query SaveDSL
saveEval (UpdatePathString str next) =
  H.modify (_pathString .~ str) $> next
