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

module SlamData.Workspace.Card.Markdown.Component
  ( markdownComponent
  , queryShouldRun
  , module SlamData.Workspace.Card.Markdown.Component.Query
  , module SlamData.Workspace.Card.Markdown.Component.State
  ) where

import SlamData.Prelude

import Data.StrMap as SM

import Data.Lens as Lens
import Data.Lens ((^?))

import DOM.BrowserFeatures.Detectors (detectBrowserFeatures)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Effects (Slam)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.CardType as Ct
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery(..))
import SlamData.Workspace.Card.Component (CardQueryP, CardStateP, makeCardComponent, makeQueryPrism', _MarkdownState, _MarkdownQuery)
import SlamData.Workspace.Card.Markdown.Component.Query (Query(..), QueryP)
import SlamData.Workspace.Card.Markdown.Component.State (State, StateP, initialState, formStateToVarMap)
import SlamData.Workspace.Card.Port as Port
import SlamData.Render.CSS as CSS

import Text.Markdown.SlamDown.Halogen.Component as SD

markdownComponent
  :: CID.CardId
  -> H.Component CardStateP CardQueryP Slam
markdownComponent cardId = makeCardComponent
  { cardType: Ct.Markdown
  , component:
      H.lifecycleParentComponent
        { render: render ("card-" ++ CID.cardIdToString cardId)
        , eval
        , peek: Nothing
        , initializer: Just $ right (H.action Init)
        , finalizer: Nothing
        }
  , initialState: H.parentState initialState
  , _State: _MarkdownState
  , _Query: makeQueryPrism' _MarkdownQuery
  }

queryShouldRun ∷ ∀ a. QueryP a → Boolean
queryShouldRun = coproduct (const false) (pred ∘ H.runChildF)
  where
  pred (SD.TextBoxChanged _ _ _) = true
  pred (SD.CheckBoxChanged _ _ _ _) = true
  pred (SD.DropDownChanged _ _ _) = true
  pred (SD.RadioButtonChanged _ _ _) = true
  pred _ = false

type MarkdownHTML a =
  H.ParentHTML
    (SD.SlamDownState Port.VarMapValue)
    (CardEvalQuery ⨁ Query)
    (SD.SlamDownQuery Port.VarMapValue)
    Slam
    a
type MarkdownDSL =
  H.ParentDSL
    State
    (SD.SlamDownState Port.VarMapValue)
    (CardEvalQuery ⨁ Query)
    (SD.SlamDownQuery Port.VarMapValue)
    Slam
    Unit

render
  ∷ String
  → State
  → MarkdownHTML Unit
render formName st =
  case st.browserFeatures of
    Nothing → HH.div_ []
    Just browserFeatures → do
      HH.div
        [ HP.class_ CSS.form ]
        [ HH.slot unit \_ →
            { component: SD.slamDownComponent { formName, browserFeatures }
            , initialState: SD.emptySlamDownState
            }
        ]

eval ∷ (CardEvalQuery ⨁ Query) ~> MarkdownDSL
eval = evalCEQ ⨁ evalQ

evalQ ∷ Query ~> MarkdownDSL
evalQ (Init next) = do
  browserFeatures ← H.fromEff detectBrowserFeatures
  H.modify (_ { browserFeatures = Just browserFeatures })
  pure next

evalCEQ ∷ CardEvalQuery ~> MarkdownDSL
evalCEQ (EvalCard info output next) = do
  for_ (info.input ^? Lens._Just ∘ Port._SlamDown) \sd → do
    H.modify (_ { input = Just sd })
    void $ H.query unit $ H.action (SD.SetDocument sd)
  pure next
evalCEQ (Save k) = do
  input ← fromMaybe mempty <$> H.gets _.input
  state ← fromMaybe SM.empty <$> H.query unit (H.request SD.GetFormState)
  pure ∘ k $ Card.Markdown { input, state }
evalCEQ (Load card next) = do
  case card of
    Card.Markdown { input, state } →
      void $ do
        H.modify (_ { input = Just input })
        H.query unit $ H.action (SD.SetDocument input)
        H.query unit $ H.action (SD.PopulateForm state)
    _ → pure unit
  pure next
evalCEQ (SetDimensions _ next) = pure next
