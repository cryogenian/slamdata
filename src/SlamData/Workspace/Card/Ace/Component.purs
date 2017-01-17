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

module SlamData.Workspace.Card.Ace.Component
  ( aceComponent
  , DSL
  , HTML
  , module SlamData.Workspace.Card.Ace.Component.Query
  , module SlamData.Workspace.Card.Ace.Component.State
  ) where

import SlamData.Prelude

import Ace.Editor as Editor
import Ace.EditSession as Session
import Ace.Halogen.Component as AC
import Ace.Types (Editor)

import Control.Monad.Aff.AVar (makeVar, takeVar)
import Control.Monad.Aff.EventLoop as EventLoop

import Data.Array as Array
import Data.Lens ((.~))
import Data.String as Str
import Data.StrMap as SM

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B
import Halogen.Component.Utils (subscribeToASource')

import SlamData.Monad (Slam)
import SlamData.Notification as N
import SlamData.Render.Common (glyph)
import SlamData.Render.CSS as CSS
import SlamData.Workspace.Card.Ace.Component.Query (Query(..), QueryP)
import SlamData.Workspace.Card.Ace.Component.State (State, StateP, initialState, _levelOfDetails)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

import Utils.Ace (getRangeRecs, readOnly)

type DSL = H.ParentDSL State AC.AceState (CC.CardEvalQuery ⨁ Query) AC.AceQuery Slam Unit
type HTML = H.ParentHTML AC.AceState (CC.CardEvalQuery ⨁ Query) AC.AceQuery Slam Unit

aceComponent ∷ CT.AceMode → CC.CardOptions → H.Component CC.CardStateP CC.CardQueryP Slam
aceComponent mode options = CC.makeCardComponent
  { options
  , cardType: CT.Ace mode
  , component: H.lifecycleParentComponent
      { render: render mode
      , eval: eval mode
      , peek: Just (peek ∘ H.runChildF)
      , initializer: Just $ right $ H.action Init
      , finalizer: Just $ right $ H.action Finalize
      }
  , initialState: H.parentState initialState
  , _State: CC._AceState
  , _Query: CC.makeQueryPrism' CC._AceQuery
  }

eval ∷ CT.AceMode → (CC.CardEvalQuery ⨁ Query) ~> DSL
eval mode = cardEval mode ⨁ evalComponent

evalComponent ∷ Query ~> DSL
evalComponent = case _ of
  Init next → do
    trigger ← H.fromAff $ makeVar
    breaker ← subscribeToASource'
      (const $ right $ H.action RunFromNotification)
      (takeVar trigger)
    H.modify (_ { trigger = Just trigger, breaker = Just breaker })
    pure next
  RunFromNotification next → do
    CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Finalize next → do
    breaker ← H.gets _.breaker
    H.fromAff $ for_ breaker EventLoop.break'
    pure next

cardEval ∷ CT.AceMode → CC.CardEvalQuery ~> DSL
cardEval mode = case _ of
  CC.Activate next → do
    mbEditor ← H.query unit $ H.request AC.GetEditor
    for_ (join mbEditor) $ H.fromEff ∘ Editor.focus
    pure next
  CC.Deactivate next → do
    st ← H.get
    for_ st.trigger \trigger →
      when st.dirty do
        N.info "Don't forget to run your query to see the latest result."
          Nothing
          Nothing
          (Just $ N.ActionOptions
            { message: ""
            , actionMessage: "Run query now"
            , action: N.Fulfill trigger
            })
    pure next
  CC.Save k → do
    content ← fromMaybe "" <$> H.query unit (H.request AC.GetText)
    mbEditor ← H.query unit (H.request AC.GetEditor)
    rrs ← H.fromEff $ maybe (pure []) getRangeRecs $ join mbEditor
    pure ∘ k
      $ Card.Ace mode { text: content, ranges: rrs }
  CC.Load card next → do
    case card of
      Card.Ace _ { text, ranges } → do
        H.query unit $ H.action $ AC.SetText text
        mbEditor ← H.query unit $ H.request AC.GetEditor
        H.fromEff $ for_ (join mbEditor) \editor → do
          traverse_ (readOnly editor) ranges
          Editor.navigateFileEnd editor
      _ → pure unit
    H.modify _ { dirty = false }
    pure next
  CC.ReceiveInput _ varMaps next → do
    let vars = SM.keys varMaps
    H.query unit $ H.action $ AC.SetCompleteFn \_ _ _ inp → do
      let inp' = Str.toLower inp
      pure $ flip Array.mapMaybe vars \var → do
        guard $ Str.contains (Str.Pattern inp') (Str.toLower var)
        pure
          { value: ":" <> Port.escapeIdentifier var
          , score: 200.0
          , caption: Just var
          , meta: "var"
          }
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState _ next → do
    pure next
  CC.ReceiveDimensions dims next → do
    H.modify
      $ _levelOfDetails
      .~ if dims.width < 240.0 then Low else High
    mbEditor ← H.query unit $ H.request AC.GetEditor
    for_ (join mbEditor) $ H.fromEff ∘ Editor.resize Nothing
    pure next
  CC.ModelUpdated _ next → do
    H.modify _ { dirty = false }
    pure next
  CC.ZoomIn next →
    pure next

peek ∷ ∀ x. AC.AceQuery x → DSL Unit
peek = case _ of
  AC.TextChanged _ →
    unlessM (H.gets _.dirty) do
      H.modify _ { dirty = true }
  _ → pure unit

aceSetup ∷ CT.AceMode → Editor → Slam Unit
aceSetup mode editor = H.fromEff do
  Editor.setTheme "ace/theme/chrome" editor
  Editor.setEnableLiveAutocompletion true editor
  Editor.setEnableBasicAutocompletion true editor
  Session.setMode (CT.aceMode mode) =<< Editor.getSession editor

render ∷ CT.AceMode → State → HTML
render mode state =
  HH.div_
    [ renderHighLOD mode state
    , renderLowLOD (CT.cardIconLightImg $ CT.Ace mode) left state.levelOfDetails
    ]

renderHighLOD ∷ CT.AceMode → State → HTML
renderHighLOD mode state =
  HH.div
    [ HP.classes
        $ [ CSS.cardInput, CSS.aceContainer ]
        ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    [ HH.div [ HP.class_ (HH.className "sd-ace-inset-shadow") ] []
    , HH.div
        [ HP.class_ (HH.className "sd-ace-toolbar") ]
        [ HH.button
            [ HP.class_ (HH.className "sd-ace-run")
            , HP.disabled (not state.dirty)
            , HP.title "Run Query"
            , ARIA.label "Run query"
            , HE.onClick (HE.input_ (left <<< CC.ModelUpdated CC.EvalModelUpdate))
            ]
            [ glyph B.glyphiconPlay
            , HH.text "Run Query"
            ]
        ]
    , HH.slot unit \_ →
        { component: AC.aceComponent (aceSetup mode) (Just AC.Live)
        , initialState: AC.initialAceState
        }
    ]
