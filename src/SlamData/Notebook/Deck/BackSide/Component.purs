module SlamData.Notebook.Deck.BackSide.Component where

import SlamData.Prelude

import Data.Array as Arr
import Data.Foldable as F
import Data.String as Str

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.HTML.Events.Indexed as HE

import SlamData.Effects (Slam)
import SlamData.Render.CSS as Rc
import SlamData.Render.Common (glyph)

data Query a
  = UpdateFilter String a
  | DoAction BackAction a

data BackAction
  = Trash
  | Share
  | Embed
  | Publish
  | Mirror
  | Wrap


allBackActions ∷ Array BackAction
allBackActions =
  [ Trash
  , Share
  , Embed
  , Publish
  , Mirror
  , Wrap
  ]

type State =
  { filterString ∷ String
  }

initialState ∷ State
initialState =
  { filterString: ""
  }


labelAction ∷ BackAction → String
labelAction action = case action of
  Trash → "Trash card"
  Share → "Share deck"
  Embed → "Embed deck"
  Publish → "Publish deck"
  Mirror → "Mirror"
  Wrap → "Wrap"

keywordsAction ∷ BackAction → Array String
keywordsAction Trash = ["remove", "delete", "trash"]
keywordsAction Share = ["share"]
keywordsAction Embed = ["embed"]
keywordsAction Publish = ["publish", "presentation", "view"]
keywordsAction Mirror = [] --["mirror", "copy", "duplicate", "shallow"]
keywordsAction Wrap = [] --["wrap", "pin", "card"]

actionGlyph ∷ BackAction → HTML
actionGlyph action = case action of
  Trash → glyph B.glyphiconTrash
  Share → glyph B.glyphiconShare
  Embed → glyph B.glyphiconShareAlt
  Publish → glyph B.glyphiconBlackboard
  Mirror → glyph B.glyphiconDuplicate
  Wrap → glyph B.glyphiconLogIn


type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Slam

comp ∷ H.Component State Query Slam
comp = H.component {render, eval}

render ∷ State → HTML
render state =
  HH.div
    [ HP.classes [ Rc.notebookCard, Rc.deckBackSide ] ]
    [ HH.form_
        [ HH.div_
            [ HH.input
              [ HP.value state.filterString
              , HE.onValueInput (HE.input UpdateFilter)
              , ARIA.label "Filter actions"
              , HP.placeholder "Filter actions"
              ]
            , HH.span_
                [ HH.button
                    [ HP.buttonType HP.ButtonButton ]
                    [ glyph B.glyphiconRemove ]
                ]
            ]
        ]
  , HH.ul_
      (map backsideAction spannedAction.init -- allBackActions
       ⊕ map disabledAction spannedAction.rest)
  ]
  where
  spannedAction ∷ {init ∷ Array BackAction, rest ∷ Array BackAction}
  spannedAction =
    foldl
      (\{init, rest} action →
         if backActionConforms action
           then { init: Arr.snoc init action, rest }
           else { init, rest: Arr.snoc rest action }
      )
      {init: [], rest: []}
      allBackActions

  backActionConforms ∷ BackAction → Boolean
  backActionConforms ba =
    F.any
      (isJust ∘ Str.stripPrefix (Str.trim $ Str.toLower state.filterString))
      (keywordsAction ba)
  backsideAction ∷ BackAction → HTML
  backsideAction action =
    let
      lbl = labelAction action
      icon = actionGlyph action
    in
      HH.li_
        [ HH.button
            [ HP.title lbl
            , ARIA.label lbl
            , HE.onClick (HE.input_ (DoAction action))
            ]
            [ icon
            , HH.p_ [ HH.text lbl ] ]
        ]

  disabledAction ∷ BackAction → HTML
  disabledAction action =
    let
      lbl = labelAction action
      icon = actionGlyph action
    in
      HH.li_
        [ HH.button
            [ HP.title $ lbl ⊕ " disabled"
            , ARIA.label $ lbl ⊕ " disabled"
            , HP.disabled true
            , HP.buttonType HP.ButtonButton
            ]
            [ icon
            , HH.p_ [ HH.text lbl ] ]
        ]

eval ∷ Natural Query DSL
eval (DoAction _ next) = pure next
eval (UpdateFilter str next) =
  H.modify (_{filterString = str}) $> next
