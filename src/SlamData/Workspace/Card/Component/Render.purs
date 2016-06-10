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

module SlamData.Workspace.Card.Component.Render
  ( CardHTML
  , header
  , statusBar
  ) where

import SlamData.Prelude

import Data.Int (fromNumber)
import Data.Time (Seconds(..), Milliseconds(..), toSeconds)

import Halogen (ParentHTML)
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Workspace.Card.Component.Query (CardQuery(..), InnerCardQuery)
import SlamData.Workspace.Card.Component.State (CardState, AnyCardState)
import SlamData.Workspace.Card.RunState (RunState(..), isRunning)
import SlamData.Effects (Slam)
import SlamData.Render.Common (glyph)
import SlamData.Render.CSS as CSS
import SlamData.Workspace.Card.CardType (CardType, cardName, cardGlyph, controllable)

type CardHTML = ParentHTML AnyCardState CardQuery InnerCardQuery Slam Unit

header
  ∷ CardType
  → CardState
  → Array CardHTML
header cty cs =
  guard (controllable cty) $>
    H.div
      [ P.classes [CSS.cardHeader]
      , ARIA.label $ (cardName cty) ⊕ " card"
      ]
      [ H.div
          [ P.class_ CSS.cardIcon ]
          [ cardGlyph cty ]
      , H.div
          [ P.class_ CSS.cardName ]
          [ H.text $ cardName cty ]
      ]

cardBlocked ∷ CardState → Boolean
cardBlocked cs = false -- TODO: this should be redundant -gb

hasMessages ∷ CardState → Boolean
hasMessages cs = false -- TODO: this _is_ redundant -gb

statusBar ∷ Boolean → CardState → CardHTML
statusBar hasResults cs =
  H.div
    [ P.classes [CSS.cardEvalLine] ]
    $ [ H.button
          [ P.classes [B.btn, B.btnPrimary, button.className]
          , E.onClick (E.input_ $ if isRunning cs.runState
                                  then StopCard
                                  else RunCard)
          , P.title button.label
          , P.disabled $ cardBlocked cs
          , ARIA.label button.label
          ]
          [ glyph button.glyph ]
      , H.div
          [ P.class_ CSS.statusText ]
          [ H.text $ if cardBlocked cs
                       then ""
                       else runStatusMessage cs.runState ]
      ]
  where

  button =
    if isRunning cs.runState
    then { className: CSS.stopButton, glyph: B.glyphiconStop, label: "Stop" }
    else { className: CSS.playButton, glyph: B.glyphiconRefresh, label: "Refresh" }

runStatusMessage ∷ RunState → String
runStatusMessage RunInitial = ""
runStatusMessage (RunElapsed t) =
  "Running for " ⊕ printSeconds t ⊕ "..."
runStatusMessage (RunFinished t) =
  "Finished: took " ⊕ printMilliseconds t ⊕ "."

printSeconds ∷ Milliseconds → String
printSeconds t = case toSeconds t of
  Seconds s → maybe "0" show (fromNumber (Math.floor s)) ⊕ "s"

printMilliseconds ∷ Milliseconds → String
printMilliseconds (Milliseconds ms) =
  maybe "0" show (fromNumber (Math.floor ms)) ⊕ "ms"

refreshButton ∷ CardHTML
refreshButton =
  H.button
    [ P.title "Refresh card content"
    , ARIA.label "Refresh card content"
    , P.classes [CSS.refreshButton]
    , E.onClick (E.input_ RefreshCard)
    ]
    [ glyph B.glyphiconRefresh ]
