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

import Data.Lens ((^?), (.~))
import Data.Lens as Lens
import Data.Path.Pathy (printPath)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import Quasar.Paths as Paths

import SlamData.Download.Model as D
import SlamData.Effects (Slam)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.CardType (CardType(Download))
import SlamData.Workspace.Card.Common.EvalQuery as Ec
import SlamData.Workspace.Card.Component (makeCardComponent, makeQueryPrism, _DownloadState, _DownloadQuery)
import SlamData.Workspace.Card.Component as Cc
import SlamData.Workspace.Card.Download.Component.Query (QueryP)
import SlamData.Workspace.Card.Download.Component.State (State, initialState, _url, _levelOfDetails, _fileName)
import SlamData.Workspace.Card.Port as P
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Quasar (reqHeadersToJSON, encodeURI)
import SlamData.Quasar.Auth as API

import Utils.Path as UP
import Utils.DOM (getTextWidthPure)

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
    , HP.href state.url
    , ARIA.label $ fullDownloadString state
    , HP.title $ fullDownloadString state
    ]
    [ HH.text $ buttonText state ]
  where

  buttonText ∷ State → String
  buttonText state
    | state.levelOfDetails ≡ Low = "Download"
    | otherwise = fullDownloadString state

fullDownloadString ∷ State → String
fullDownloadString state = "Download " ⊕ state.fileName

eval ∷ QueryP ~> DSL
eval = coproduct cardEval (absurd ∘ getConst)

cardEval ∷ Ec.CardEvalQuery ~> DSL
cardEval (Ec.EvalCard info output next ) = do
  for_ (info.input ^? Lens._Just ∘ P._DownloadOptions) handleDownloadPort
  pure next
cardEval (Ec.Save k) = pure (k Card.Download)
cardEval (Ec.Load json next) = pure next
cardEval (Ec.SetDimensions dims next) = do
  textWidth ← H.gets $ flip getTextWidthPure "normal 14px Ubuntu" ∘ _.fileName
  let
    buttonPadding = 24.0
    cardPadding = 24.0
    grippersWidth = 48.0
  H.modify
    $ _levelOfDetails
    .~ if dims.width < textWidth + buttonPadding + cardPadding + grippersWidth
         then Low
         else High
  pure next

handleDownloadPort ∷ P.DownloadPort → DSL Unit
handleDownloadPort opts = do
  hs ← H.fromEff API.authHeaders
  H.modify $ _url .~ url hs
  let
    fileName = UP.getNameStr $ Right opts.resource
    ext | opts.compress = ".zip"
    ext | isRight opts.options = ".json"
    ext | otherwise = ".csv"
  H.modify $ _fileName .~ (fileName ⊕ ext)
  where

  url hs =
    (encodeURI (printPath Paths.data_ ⊕ printPath opts.resource))
    ⊕ headersPart hs

  headersPart hs =
    "?request-headers="
      ⊕ (Global.encodeURIComponent
           $ show
           $ reqHeadersToJSON
           $ append hs
           $ D.toHeaders opts)
