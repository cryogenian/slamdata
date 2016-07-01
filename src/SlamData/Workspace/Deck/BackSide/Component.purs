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

module SlamData.Workspace.Deck.BackSide.Component where

import SlamData.Prelude

import Data.Array as Arr
import Data.Foldable as F
import Data.String as Str

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.Render.Common (glyph)
import SlamData.Render.CSS as Rc
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component.CSS as CCSS
import SlamData.Quasar.Auth (retrieveIdToken)

data Query a
  = UpdateFilter String a
  | DoAction BackAction a
  | UpdateCardType (Maybe CT.CardType) a
  | Init a

data BackAction
  = Trash
  | Rename
  | Share
  | Embed
  | Publish
  | DeleteDeck
  | Mirror
  | Wrap
  | Share
  | Unshare


allBackActions ∷ State → Array BackAction
allBackActions state =
  [ Trash
  , Rename
  ]
  ⊕ (if state.isLogged
      then [ Share
           , Unshare
           ]
      else [ ])
  ⊕ [ Embed
    , Publish
    , DeleteDeck
    , Mirror
    , Wrap
    ]

type State =
  { filterString ∷ String
  , cardType ∷ Maybe CT.CardType
  , saved ∷ Boolean
  , isLogged ∷ Boolean
  }

initialState ∷ State
initialState =
  { filterString: ""
  , cardType: Nothing
  , saved: false
  , isLogged: false
  }


labelAction ∷ BackAction → String
labelAction action = case action of
  Trash → "Trash card"
  Rename → "Rename deck"
  Share → "Share deck"
  Embed → "Embed deck"
  Publish → "Publish deck"
  DeleteDeck → "Delete deck"
  Mirror → "Mirror"
  Wrap → "Wrap"
  Unshare → "Unshare deck"

keywordsAction ∷ BackAction → Array String
keywordsAction Trash = ["remove", "delete", "trash"]
keywordsAction Rename = ["rename", "title"]
keywordsAction Share = ["share"]
keywordsAction Embed = ["embed"]
keywordsAction Publish = ["publish", "presentation", "view"]
keywordsAction DeleteDeck = ["remove", "delete", "trash"]
keywordsAction Mirror = ["mirror", "copy", "duplicate", "shallow"]
keywordsAction Wrap = ["wrap", "pin", "card"]
keywordsAction Unshare = ["unshare", "manage"]

actionEnabled ∷ State → BackAction → Boolean
actionEnabled st a =
  case st.cardType, a of
    Just CT.ErrorCard, Trash → false
    Just CT.NextAction, Trash → false
    Nothing, Trash → false
    _, _ → true

actionGlyph ∷ BackAction → HTML
actionGlyph = case _ of
  Trash → glyph B.glyphiconTrash
  Rename → glyph B.glyphiconPencil
  Share → glyph B.glyphiconShare
  Unshare → glyph B.glyphiconWrench
  Embed → glyph B.glyphiconShareAlt
  Publish → glyph B.glyphiconBlackboard
  Mirror → glyph B.glyphiconDuplicate
  Wrap → glyph B.glyphiconLogIn
  DeleteDeck → HH.i [ HP.classes [ Rc.actionIcon, Rc.deleteDeckIcon ] ] [ ]

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Slam

comp ∷ H.Component State Query Slam
comp =
  H.lifecycleComponent
    { render
    , eval
    , finalizer: Nothing
    , initializer: Just (H.action Init)
    }

render ∷ State → HTML
render state =
  HH.div
    [ HP.class_ CCSS.deckCard ]
    [ HH.div
        [ HP.class_ Rc.deckBackSide ]
        [ HH.div_
            [ HH.form_
                [ HH.div_
                    [ HH.input
                        [ HP.value state.filterString
                        , HE.onValueInput (HE.input UpdateFilter)
                        , ARIA.label "Filter actions"
                        , HP.placeholder "Filter actions"
                        ]
                    , HH.button
                          [ HP.buttonType HP.ButtonButton ]
                          [ glyph B.glyphiconRemove ]
                    ]
                ]
            , HH.ul_
                $ map (backsideAction true) actions.enabledActions
                ⊕ map (backsideAction false) actions.disabledActions
            ]
        ]
    ]
  where

  actions ∷ {enabledActions ∷ Array BackAction, disabledActions ∷ Array BackAction}
  actions =
    foldl
      (\{enabledActions, disabledActions} action →
         if backActionConforms action
           then { enabledActions: Arr.snoc enabledActions action, disabledActions }
           else { enabledActions, disabledActions: Arr.snoc disabledActions action }
      )
      {enabledActions: [], disabledActions: []}
      (allBackActions state)

  backActionConforms ∷ BackAction → Boolean
  backActionConforms ba =
    actionEnabled state ba &&
      F.any
        (isJust ∘ Str.stripPrefix (Str.trim $ Str.toLower state.filterString))
        (keywordsAction ba)

  backsideAction ∷ Boolean → BackAction → HTML
  backsideAction enabled action =
    HH.li_
      [ HH.button attrs
          [ icon
          , HH.p_ [ HH.text $ labelAction action ]
          ]
      ]
    where
      attrs =
        [ HP.title lbl
        , ARIA.label lbl
        , HP.disabled (not enabled)
        , HP.buttonType HP.ButtonButton
        ] ⊕ if enabled then [ HE.onClick (HE.input_ (DoAction action)) ] else [ ]

      lbl = labelAction action ⊕ if enabled then "" else " disabled"
      icon = actionGlyph action

eval ∷ Query ~> DSL
eval (DoAction _ next) = pure next
eval (UpdateFilter str next) =
  H.modify (_ { filterString = str }) $> next
eval (UpdateCardType cty next) =
  H.modify (_ { cardType = cty }) $> next
eval (Init next) = next <$ do
  isLogged ← map isJust $ H.fromEff retrieveIdToken
  H.modify (_ { isLogged = isLogged })
