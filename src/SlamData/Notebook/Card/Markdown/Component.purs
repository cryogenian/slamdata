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

module SlamData.Notebook.Card.Markdown.Component
  ( markdownComponent
  , queryShouldRun
  , module SlamData.Notebook.Card.Markdown.Component.Query
  , module SlamData.Notebook.Card.Markdown.Component.State
  ) where

import SlamData.Prelude

import Data.BrowserFeatures (BrowserFeatures)
import Data.StrMap as SM

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Effects (Slam)
import SlamData.Notebook.Card.CardId (CardId, runCardId)
import SlamData.Notebook.Card.CardType as Ct
import SlamData.Notebook.Card.Common.EvalQuery (CardEvalQuery(..), CardEvalResult)
import SlamData.Notebook.Card.Component (CardQueryP, CardStateP, makeCardComponent, makeQueryPrism, _MarkdownState, _MarkdownQuery)
import SlamData.Notebook.Card.Markdown.Component.Query (QueryP)
import SlamData.Notebook.Card.Markdown.Component.State (State, StateP, initialState, formStateToVarMap)
import SlamData.Notebook.Card.Markdown.Model (decode, encode)
import SlamData.Notebook.Card.Port as Port
import SlamData.Render.CSS as CSS

import Text.Markdown.SlamDown.Halogen.Component as SD

markdownComponent
  :: CardId
  -> BrowserFeatures
  -> H.Component CardStateP CardQueryP Slam
markdownComponent cardId browserFeatures = makeCardComponent
  { cardType: Ct.Markdown
  , component: H.parentComponent { render: render config, eval, peek: Nothing }
  , initialState: H.parentState initialState
  , _State: _MarkdownState
  , _Query: makeQueryPrism _MarkdownQuery
  }
  where
  config ∷ SD.SlamDownConfig
  config =
    { formName: "card-" ++ show (runCardId cardId)
    , browserFeatures
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
    CardEvalQuery
    (SD.SlamDownQuery Port.VarMapValue)
    Slam
    a
type MarkdownDSL =
  H.ParentDSL
    State
    (SD.SlamDownState Port.VarMapValue)
    CardEvalQuery
    (SD.SlamDownQuery Port.VarMapValue)
    Slam
    Unit

render
  ∷ ∀ a
  . SD.SlamDownConfig
  → a
  → MarkdownHTML Unit
render config _ =
  HH.div
    [ HP.class_ CSS.markdownOutput ]
    [ HH.slot unit \_ →
        { component: SD.slamDownComponent config
        , initialState: SD.emptySlamDownState
        }
    ]

eval ∷ Natural CardEvalQuery MarkdownDSL
eval (NotifyRunCard next) = pure next
eval (NotifyStopCard next) = pure next
eval (EvalCard value k) =
  case value.inputPort of
    Just (Port.SlamDown input) → do
      H.set $ Just input
      H.query unit $ H.action (SD.SetDocument input)
      let desc = SD.formDescFromDocument input
      state ← H.query unit $ H.request SD.GetFormState
      k <$>
        case state of
          Nothing → pure $ error "GetFormState query returned Nothing"
          Just st → do
            varMap ← H.liftH ∘ H.liftH $ formStateToVarMap desc st
            pure { output: Just $ Port.VarMap varMap, messages: [] }
    _ → pure ∘ k $ error "expected SlamDown input"
eval (SetupCard _ next) = pure next
eval (Save k) = do
  input ← fromMaybe mempty <$> H.get
  state ← fromMaybe SM.empty <$> H.query unit (H.request SD.GetFormState)
  pure $ k (encode { input, state })
eval (Load json next) = do
  case decode json of
    Right { input, state } →
      void $ do
        H.set $ Just input
        H.query unit $ H.action (SD.SetDocument input)
        H.query unit $ H.action (SD.PopulateForm state)
    _ → pure unit
  pure next
eval (SetCanceler _ next) = pure next

error :: String -> CardEvalResult
error msg =
  { output: Nothing
  , messages: [Left $ "An internal error occurred: " ++ msg]
  }
