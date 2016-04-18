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

module SlamData.Notebook.Card.Search.Component
  ( searchComponent
  , module SlamData.Notebook.Card.Search.Component.Query
  , module SlamData.Notebook.Card.Search.Component.State
  ) where

import SlamData.Prelude

import Control.Monad.Error.Class as EC
import Control.Monad.Writer.Class as WC

import Data.Argonaut (encodeJson, decodeJson)
import Data.Lens ((.~), preview)
import Data.StrMap as SM

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import Quasar.Aff as Quasar
import Quasar.Auth as Auth

import SlamData.Effects (Slam)
import SlamData.FileSystem.Resource as R
import SlamData.Notebook.Card.CardType as CT
import SlamData.Notebook.Card.Common.EvalQuery (liftWithCanceler', temporaryOutputResource, runCardEvalT)
import SlamData.Notebook.Card.Component as NC
import SlamData.Notebook.Card.Port as Port
import SlamData.Notebook.Card.Search.Component.Query (Query, SearchQuery(..))
import SlamData.Notebook.Card.Search.Component.State (State, _running, _searchString, initialState)
import SlamData.Notebook.Card.Search.Interpret as Search
import SlamData.Render.Common as RC
import SlamData.Render.CSS as CSS

import Text.SlamSearch as SS

type DSL = H.ComponentDSL State Query Slam
type HTML = H.ComponentHTML Query

searchComponent ∷ H.Component NC.CardStateP NC.CardQueryP Slam
searchComponent =
  NC.makeCardComponent
    { cardType: CT.Search
    , component: H.component { render, eval }
    , initialState: initialState
    , _State: NC._SearchState
    , _Query: NC.makeQueryPrism NC._SearchQuery
    }

render ∷ State → HTML
render state =
  HH.div
    [ HP.classes
        [ CSS.exploreCardEditor
        , CSS.cardInput
        ]
    ]
    [
      HH.div
        [ HP.classes [ CSS.fileListField, B.inputGroup ] ]
        [ HH.input
            [ HP.classes [ B.formControl, CSS.searchCardInput ]
            , HP.placeholder "Input search string"
            , HE.onValueInput $ HE.input \str → UpdateSearch str ⋙ right
            , HP.value state.searchString
            ]
        , HH.img
            [ HE.onClick (HE.input_ $ UpdateSearch "" ⋙ right)
            , HP.class_ CSS.searchClear
            , HP.src $ if state.running then "img/spin.gif" else "img/remove.svg"
            ]
        , HH.span
            [ HP.class_ B.inputGroupBtn ]
            [ HH.button
                [ HP.classes [ B.btn, B.btnDefault, CSS.searchCardButton ]
                , HP.buttonType HP.ButtonButton
                , HE.onClick (HE.input_ $ NC.NotifyRunCard ⋙ left)
                ]
                [ RC.glyph B.glyphiconSearch
                ]
            ]
        ]
    ]

eval ∷ Natural Query DSL
eval = coproduct cardEval searchEval

cardEval ∷ Natural NC.CardEvalQuery DSL
cardEval q =
  case q of
    NC.EvalCard { inputPort: Just Port.Blocked } k → do
      pure $ k { output: Nothing, messages: [] }
    NC.EvalCard info@{ inputPort: Just (Port.TaggedResource {tag, resource})} k →
      k <$> runCardEvalT do
        query ←
          H.get <#> _.searchString ⋙ SS.mkQuery
            # lift
            >>= either (\_ → EC.throwError "Incorrect query string") pure

        Quasar.messageIfResourceNotExists
            resource
            ("Input resource " ⊕ R.resourcePath resource ⊕ " doesn't exist")
          # Auth.authed
          # liftWithCanceler'
          # lift
          >>= traverse_ EC.throwError
        fields ←
          Quasar.fields resource
            # Auth.authed
            # liftWithCanceler'
            # lift

        let
          template = Search.queryToSQL fields query
          sql = Quasar.templated resource template
          tempOutputResource = R.Mount $ R.View $ temporaryOutputResource info

        WC.tell ["Generated SQL: " ⊕ sql]

        { plan, outputResource } ←
          Quasar.executeQuery
            template
            false
            SM.empty
            resource
            tempOutputResource
            # Auth.authed
            # liftWithCanceler'
            # lift
            >>= either (\err → EC.throwError $ "Error in query: " ⊕ err) pure

        for_ plan \p → WC.tell ["Plan: " ⊕ p]
        Quasar.messageIfResourceNotExists
            outputResource
            "Error making search temporary resource"
          # Auth.authed
          # liftWithCanceler'
          # lift
          >>= traverse_ EC.throwError
        pure $ Port.TaggedResource { resource: outputResource, tag: pure sql }
    NC.EvalCard _ k →
      pure $ k { output: Nothing, messages: [Left  "expected a Resource input"] }
    NC.SetupCard _ next →
      pure next
    NC.NotifyRunCard next →
      pure next
    NC.NotifyStopCard next →
      pure next
    NC.Save k → do
      input ← H.gets _.searchString
      pure $ k $ encodeJson input
    NC.Load json next → do
      for_ (decodeJson json) \input →
        H.modify $ _searchString .~ input
      pure next
    NC.SetCanceler _ next → pure next

searchEval ∷ Natural SearchQuery DSL
searchEval (UpdateSearch str next) =
  H.modify (_searchString .~ str) $> next
