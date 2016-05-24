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
∘
-}

module SlamData.Workspace.Card.Download.Component where

import SlamData.Prelude

import Control.Monad.Error.Class (throwError)

import Data.Argonaut (jsonEmptyObject)
import Data.Path.Pathy (printPath)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import Quasar.Paths as Paths

import SlamData.Download.Model as D
import SlamData.Effects (Slam)
import SlamData.Workspace.Card.CardType (CardType(Download))
import SlamData.Workspace.Card.Common.EvalQuery as Ec
import SlamData.Workspace.Card.Component (makeCardComponent, makeQueryPrism, _DownloadState, _DownloadQuery)
import SlamData.Workspace.Card.Component as Cc
import SlamData.Workspace.Card.Download.Component.Query (QueryP)
import SlamData.Workspace.Card.Download.Component.State (State, initialState)
import SlamData.Workspace.Card.Port as P
import SlamData.Quasar (reqHeadersToJSON, encodeURI)

type HTML = H.ComponentHTML QueryP
type DSL = H.ComponentDSL State QueryP Slam

downloadComponent ∷ Cc.CardComponent
downloadComponent = makeCardComponent
  { cardType: Download
  , component: H.component { render, eval }
  , initialState: initialState
  , _State: _DownloadState
  , _Query: makeQueryPrism _DownloadQuery
  }

render ∷ State → HTML
render state =
  HH.a
    [ HP.classes
        [ B.btn
        , B.btnPrimary
        , HH.className "download-button"
        ]
    , HP.href state
    , ARIA.label "Download"
    , HP.title "Download"
    ]
    [ HH.text "Download" ]

eval ∷ QueryP ~> DSL
eval = coproduct cardEval (absurd ∘ getConst)

cardEval ∷ Ec.CardEvalQuery ~> DSL
cardEval (Ec.EvalCard { inputPort } continue) = do
  map continue $ Ec.runCardEvalT do
    case inputPort of
      Just P.Blocked → lift $ pure $ Just P.Blocked
      Just (P.DownloadOptions opts) → lift do
        handleDownloadPort opts
        pure $ Just P.Blocked
      _ → throwError "Incorrect input in download card"
cardEval (Ec.NotifyRunCard next) = pure next
cardEval (Ec.NotifyStopCard next) = pure next
cardEval (Ec.Save k) = pure $ k jsonEmptyObject
cardEval (Ec.Load json next) = pure next
cardEval (Ec.SetupCard { inputPort } next) = do
  case inputPort of
    P.DownloadOptions opts → do
      handleDownloadPort opts
      pure unit
    _ → pure unit
  pure next
cardEval (Ec.SetCanceler _ next) = pure next
cardEval (Ec.SetDimensions _ next) = pure next

handleDownloadPort ∷ P.DownloadPort → DSL Unit
handleDownloadPort opts = do
  H.set url
  where
  url ∷ String
  url =
    (encodeURI (printPath Paths.data_ ⊕ printPath opts.resource))
    ⊕ headersPart

  headersPart ∷ String
  headersPart =
    "?request-headers="
      ⊕ (Global.encodeURIComponent
           $ show
           $ reqHeadersToJSON
           $ D.toHeaders opts)
