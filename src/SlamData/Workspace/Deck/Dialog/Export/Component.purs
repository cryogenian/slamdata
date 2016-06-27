module SlamData.Workspace.Deck.Dialog.Export.Component where

import SlamData.Prelude

import Control.UI.Browser (select, locationString)
import Control.UI.ZClipboard as Z

import Data.Foldable as F
import Data.StrMap as SM

import DOM.HTML.Types (HTMLElement, htmlElementToElement)

import Halogen as H
import Halogen.CustomProps as CP
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.HTML.Renderer.String (renderHTML)
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Render.CSS as Rc
import SlamData.Workspace.Routing (mkWorkspaceURL)
import SlamData.Workspace.Action as WA
import SlamData.Workspace.AccessType as AT

import Utils.Path (DirPath)

data PresentAs = IFrame | URI

derive instance eqPresentAs ∷ Eq PresentAs

data DialogStep = Confirmation | Copying

derive instance eqDialogStep ∷ Eq DialogStep

type State =
  { presentingAs ∷ PresentAs
  , varMap ∷ Port.VarMap
  , deckPath ∷ DirPath
  , step ∷ DialogStep
  , locationString ∷ Maybe String
  }

initialState ∷ DirPath → State
initialState =
  { presentingAs: URI
  , varMap: SM.empty
  , deckPath: _
  , step: Confirmation
  , locationString: Nothing
  }

data Query a
  = SelectElement HTMLElement a
  | Init String (Maybe HTMLElement) a
  | Dismiss a
  | Confirm a

comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → H.ComponentHTML Query
render state | state.step ≡ Confirmation = renderConfirm state
render state | otherwise = renderCopying state

renderConfirm ∷ State → H.ComponentHTML Query
renderConfirm _ =
  HH.div_
    [ HH.div [ HP.classes [ B.alert, B.alertInfo ] ]
        [ HH.p_ [ HH.text "You're going to publish current workspace to arbitrary large audience" ]
        ]
    , HH.div [ HP.classes [ HH.className "deck-dialog-footer" ] ]
        [ HH.button
            [ HP.classes [ B.btn ]
            , HE.onClick (HE.input_ Dismiss)
            ]
            [ HH.text "Dismiss" ]
        , HH.button
            [ HP.classes [ B.btn, B.btnPrimary ]
            , HE.onClick (HE.input_ Confirm)
            ]
            [ HH.text "Confirm" ]
        ]
    ]

renderCopying ∷ State → H.ComponentHTML Query
renderCopying state | state.presentingAs ≡ URI = renderPublishURI state
renderCopying state | otherwise = renderPublishIFrame state

renderURL ∷ State → String
renderURL {locationString, deckPath, varMap} =
  foldMap (_ ⊕ "/") locationString
  ⊕ mkWorkspaceURL deckPath (WA.Load AT.ReadOnly)
  ⊕ if SM.isEmpty varMap
      then ""
      else "/?" ⊕ F.intercalate "&" (map printArg $ SM.toList varMap)
  where
  printArg (k × v) = k ⊕ "=" ⊕ "\"" ⊕ (Global.encodeURIComponent $ Port.renderVarMapValue v ⊕ "\"")

renderPublishURI ∷ State → H.ComponentHTML Query
renderPublishURI state =
  HH.div [ HP.classes [ HH.className "deck-dialog-share" ] ]
    [ HH.h4_ [ HH.text "URL" ]
    , HH.div [ HP.classes [ HH.className "deck-dialog-body" ] ]
        [ HH.form
            [ CP.nonSubmit ]
            [ HH.div
                [ HP.classes [ B.formGroup ]
                , HE.onClick $ HE.input (SelectElement ∘ _.target)
                ]
                [ HH.input
                  [ HP.classes [ B.formControl ]
                  , HP.value $ renderURL state
                  , HP.readonly true
                  , HP.title "Publish URL"
                  , ARIA.label "Publish URL"
                  ]
                ]
            ]
        ]
    , HH.div [ HP.classes [ HH.className "deck-dialog-footer" ] ]
        [ HH.button
            [ HP.classes [ B.btn ]
            , HE.onClick (HE.input_ Dismiss)
            , HP.buttonType HP.ButtonButton
            ]
            [ HH.text "Dismiss" ]
        , HH.button
            [ HP.classes [ B.btn, B.btnPrimary ]
            , HE.onClick (HE.input_ Dismiss)
            , HP.ref (H.action ∘ Init (renderURL state))
            , HP.id_ "copy-button"
            , HP.buttonType HP.ButtonButton
            ]
            [ HH.text "Copy" ]
        , HH.a
            [ HP.classes [ B.btn, B.btnInfo ]
            , HP.target "_blank"
            , HP.href $ renderURL state
            ]
            [ HH.text "Preview" ]
        ]
    ]


renderPublishIFrame ∷ State → H.ComponentHTML Query
renderPublishIFrame state =
  HH.div [ HP.classes [ HH.className "deck-dialog-embed" ] ]
    [ HH.h4_ [ HH.text  "Embed deck" ]
    , HH.div [ HP.classes [ HH.className "deck-dialog-body" ] ]
        [ HH.form
          [ CP.nonSubmit ]
          [ HH.div
              [ HP.classes [ B.formGroup ]
              , HE.onClick $ HE.input (SelectElement ∘ _.target)
              ]
              [ HH.textarea
                  [ HP.classes [ B.formControl, Rc.embedBox ]
                  , HP.readonly true
                  , HP.value code
                  ]
              ]
          ]
        ]
    , HH.div [ HP.classes [ HH.className "deck-dialog-footer" ] ]
        [ HH.button
            [ HP.classes [ B.btn ]
            , HE.onClick (HE.input_ Dismiss)
            ]
            [ HH.text "Dismiss" ]
        , HH.button
            [ HP.id_ "copy-button"
            , HP.classes [ B.btn, B.btnPrimary ]
            , HE.onClick (HE.input_ Dismiss)
            , HP.ref (H.action ∘ Init code)
            ]
            [ HH.text "Copy"
            ]
        ]
    ]
  where
  code ∷ String
  code =
    renderHTML $
      HH.script
        [ HP.mediaType { type: "text", subtype: "javascript", parameters: [] } ]
        [ HH.text javascriptCode ]

  javascriptCode ∷ String
  javascriptCode =
    "document.writeln(\"<iframe "
    ⊕ "width=\\\"100%\\\" height=\\\"100%\\\" frameborder=\\\"0\\\" "
    ⊕ "src=\\\"" ⊕ renderURL state ⊕ "\\\"></iframe>̈\");"



eval ∷ Query ~> (H.ComponentDSL State Query Slam)
eval (Dismiss next) = pure next
eval (Init code mbEl next) =
  next <$ do
    locString ← H.fromEff locationString
    H.modify $ _{locationString = Just locString}
    -- Note, that `code` hasn't locString yet
    for_ mbEl \htmlEl →
      H.fromEff
        $ Z.make (htmlElementToElement htmlEl)
        >>= Z.onCopy (Z.setData "text/plain" $ locString ⊕ "/" ⊕ code)
eval (SelectElement el next) = H.fromEff (select el) $> next
eval (Confirm next) = H.modify _{step = Copying} $> next
