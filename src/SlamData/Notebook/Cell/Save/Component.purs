module SlamData.Notebook.Cell.Save.Component
  ( saveCellComponent
  , module SlamData.Notebook.Cell.Save.Component.State
  , module SlamData.Notebook.Cell.Save.Component.Query
  ) where


import SlamData.Prelude

import Control.Monad.Error.Class as EC
import Control.Monad.Writer.Class as WC

import Data.Argonaut (decodeJson, encodeJson)
import Data.Lens ((.~))
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pt
import Data.StrMap as Sm

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B
import Halogen.HTML.Events.Indexed as HE

import SlamData.FileSystem.Resource as R
import SlamData.Notebook.Cell.Common.EvalQuery as Eq
import SlamData.Notebook.Cell.CellType as Ct
import SlamData.Notebook.Cell.Component as Cc
import SlamData.Notebook.Cell.Port as P
import SlamData.Effects (Slam)
import SlamData.Notebook.Cell.Save.Component.State (State, initialState, _pathString)
import SlamData.Notebook.Cell.Save.Component.Query (Query(..), QueryP)
import SlamData.Render.CSS as Rc

import Quasar.Aff as Api
import Quasar.Auth as Auth

import Utils.Path as Up

type SaveHTML = H.ComponentHTML QueryP
type SaveDSL = H.ComponentDSL State QueryP Slam


saveCellComponent ∷ Cc.CellComponent
saveCellComponent = Cc.makeCellComponent
  { cellType: Ct.Save
  , component: H.component { render, eval }
  , initialState: initialState
  , _State: Cc._SaveState
  , _Query: Cc.makeQueryPrism Cc._SaveQuery
  }

render ∷ State → SaveHTML
render state =
  HH.div
    [ HP.classes
        [ Rc.exploreCellEditor
        , Rc.cellInput
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
              , HE.onClick (HE.input_ $ left ∘ Cc.NotifyRunCell)
              ]
              [ HH.text "Save" ]
            ]
        ]
    ]

eval ∷ Natural QueryP SaveDSL
eval = coproduct cellEval saveEval

cellEval ∷ Natural Eq.CellEvalQuery SaveDSL
cellEval (Eq.EvalCell info k) = case info.inputPort of
  Just P.Blocked →
    pure $ k { output: Nothing, messages: [ ] }
  Just (P.TaggedResource {tag, resource}) → do
    pt ← H.gets _.pathString
    case pt, Up.parseAnyPath pt of
      "", _ →
        pure $ k { output: Nothing, messages: [ ] }
      _, Just (Left fp) → map k $ Eq.runCellEvalT do
        {plan, outputResource} ←
          Api.executeQuery
              "select * from {{path}}"
              true
              Sm.empty
              resource
              (R.File fp)
           # Auth.authed
           # Eq.liftWithCanceler'
           # lift
           >>= either EC.throwError pure
        Api.messageIfResourceNotExists
          outputResource
          "Error saving file, please, try another location"
          # Auth.authed
          # Eq.liftWithCanceler'
          # lift
          >>= traverse_ EC.throwError
        when (R.File fp ≠ outputResource)
          $ EC.throwError
          $ "Resource: " ⊕ R.resourcePath outputResource ⊕ " hasn't been modified"
        WC.tell ["Resource successfully saved as: " ⊕ Pt.printPath fp]
        pure $ P.TaggedResource {resource: outputResource, tag: Nothing}
      _, _ →
        pure $ k { output: Just P.Blocked
                 , messages: [ Left $ pt ⊕ " is incorrect file path" ] }

  _ →
    pure $ k { output: Nothing, messages: [ Left "Expected Resource input" ] }
cellEval (Eq.NotifyRunCell next) = pure next
cellEval (Eq.Save k) = do
  pt ← H.gets _.pathString
  case Pt.parseAbsFile pt of
    Nothing → pure $ k $ encodeJson ""
    Just _ → pure $ k $ encodeJson pt
cellEval (Eq.Load js next) = do
  for_ (decodeJson js) \s → H.modify (_pathString .~ s)
  pure next
cellEval (Eq.SetupCell p next) = do
  H.modify (_pathString .~ (Pt.printPath $ Eq.temporaryOutputResource p))
  pure next
cellEval (Eq.SetCanceler _ next) = pure next

saveEval ∷ Natural Query SaveDSL
saveEval (UpdatePathString str next) =
  H.modify (_pathString .~ str) $> next
