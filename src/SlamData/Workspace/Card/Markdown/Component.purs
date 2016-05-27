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

import Control.Monad.Error.Class as Err

import Data.StrMap as SM

import DOM.BrowserFeatures.Detectors (detectBrowserFeatures)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Effects (Slam)
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.CardType as Ct
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery(..), runCardEvalT)
import SlamData.Workspace.Card.Component (CardQueryP, CardStateP, makeCardComponent, makeQueryPrism', _MarkdownState, _MarkdownQuery)
import SlamData.Workspace.Card.Markdown.Component.Query (Query(..), QueryP)
import SlamData.Workspace.Card.Markdown.Component.State (State, StateP, initialState, formStateToVarMap)
import SlamData.Workspace.Card.Markdown.Model (decode, encode)
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
evalCEQ (NotifyRunCard next) = pure next
evalCEQ (NotifyStopCard next) = pure next
evalCEQ (EvalCard value k) =
  k <$> runCardEvalT do
    case value.input of
      Just (Port.SlamDown input) → do
        lift $ H.modify (_ { input = Just input })
        lift ∘ H.query unit $ H.action (SD.SetDocument input)
        let desc = SD.formDescFromDocument input
        state ← lift ∘ H.query unit $ H.request SD.GetFormState
        case state of
          Nothing →
            Err.throwError "An internal error occured: GetFormState query returned Nothing"
          Just st → do
            varMap ← lift ∘ H.liftH ∘ H.liftH $ formStateToVarMap desc st
            pure $ Port.VarMap varMap
      _ → Err.throwError "Expected SlamDown input"
evalCEQ (SetupCard _ next) = pure next
evalCEQ (Save k) = do
  input ← fromMaybe mempty <$> H.gets _.input
  state ← fromMaybe SM.empty <$> H.query unit (H.request SD.GetFormState)
  pure $ k (encode { input, state })
evalCEQ (Load json next) = do
  case decode json of
    Right { input, state } →
      void $ do
        H.modify (_ { input = Just input })
        H.query unit $ H.action (SD.SetDocument input)
        H.query unit $ H.action (SD.PopulateForm state)
    _ → pure unit
  pure next
evalCEQ (SetCanceler _ next) = pure next
evalCEQ (SetDimensions _ next) = pure next
