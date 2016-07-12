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

module SlamData.Workspace.Card.Table.Component.Render (render) where

import SlamData.Prelude

import Data.Array as A
import Data.Char (fromCharCode)
import Data.Int as Int
import Data.Json.JTable as JT
import Data.String (fromChar)

import Halogen as H
import Halogen.HTML.Events.Handler as HEH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Render.Common (glyph)
import SlamData.Render.CSS.New as CSS
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Table.Component.Query (QueryP, PageStep(..), Query(..))
import SlamData.Workspace.Card.Table.Component.State (State, currentPageInfo)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

-- | A value that holds all possible states for an inputtable value: the current
-- | actual value, and a possible pending user-entered or selected value.
type InputValue a =
  { current ∷ a
  , pending ∷ Maybe (Either String a)
  }

-- | Converts an `InputValue` into a string for use as a HTML form field value.
fromInputValue ∷ ∀ a. Show a ⇒ InputValue a → String
fromInputValue { current, pending } =
  case pending of
    Nothing → show current
    Just pending' → either id show pending'

render ∷ State → H.ComponentHTML QueryP
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.lightCardGlyph CT.Table) left state.levelOfDetails
    ]

renderHighLOD ∷ State → H.ComponentHTML QueryP
renderHighLOD st =
  HH.div
    [ HP.classes $ (guard (st.levelOfDetails ≠ High) $> B.hidden) ]
    $ flip foldMap st.result \result →
        let
          p = currentPageInfo st
        in
          [ right <$> JT.renderJTable jTableOpts result.json
          , HH.div
            [ HP.classes [CSS.pagination, CSS.form] ]
            [ prevButtons (p.page <= 1)
            , pageField { current: p.page, pending: st.page } p.totalPages
            , nextButtons (p.page >= p.totalPages)
            , pageSizeControls st.isEnteringPageSize { current: p.pageSize, pending: st.pageSize }
            ]
          ]

jTableOpts ∷ JT.JTableOpts
jTableOpts = JT.jTableOptsDefault
  { style = JT.noStyle
  , columnOrdering = JT.alphaOrdering
  }

prevButtons ∷ Boolean → H.ComponentHTML QueryP
prevButtons enabled =
  HH.div
    [ HP.class_ CSS.formButtonGroup ]
    [ HH.button
        [ HP.class_ CSS.formButton
        , HP.disabled enabled
        , HE.onClick $ HE.input_ (right ∘ StepPage First)
        ]
        [ glyph B.glyphiconFastBackward ]
    , HH.button
        [ HP.class_ CSS.formButton
        , HP.disabled enabled
        , HE.onClick $ HE.input_ (right ∘ StepPage Prev)
        ]
        [ glyph B.glyphiconStepBackward ]
    ]

pageField ∷ InputValue Int → Int → H.ComponentHTML QueryP
pageField pageValue totalPages =
  HH.div_
    [ submittable
        [ HH.text "Page"
        , HH.input
            [ HP.inputType HP.InputNumber
            , HP.value (fromInputValue pageValue)
            , HE.onValueInput (HE.input (\x → right ∘ SetCustomPage x))
            ]
        , HH.text $ "of " <> show totalPages
        ]
    ]

submittable ∷ Array (H.ComponentHTML QueryP) → H.ComponentHTML QueryP
submittable =
  HH.form
    [ HE.onSubmit \_ →
        HEH.preventDefault $> Just (H.action (right ∘ Update))
    ]

nextButtons ∷ Boolean → H.ComponentHTML QueryP
nextButtons enabled =
  HH.div
    [ HP.class_ CSS.formButtonGroup ]
    [ HH.button
        [ HP.disabled enabled
        , HE.onClick $ HE.input_ (right ∘ StepPage Next)
        ]
        [ glyph B.glyphiconStepForward ]
    , HH.button
        [ HP.disabled enabled
        , HE.onClick $ HE.input_ (right ∘ StepPage Last)
        ]
        [ glyph B.glyphiconFastForward ]
    ]

pageSizeControls ∷ Boolean → InputValue Int → H.ComponentHTML QueryP
pageSizeControls showCustom pageSize =
  HH.div_
    [ submittable
         $ [ HH.text "Per page:" ]
         ⊕ [ if showCustom
             then HH.input
                [ HP.inputType HP.InputNumber
                , HP.value (fromInputValue pageSize)
                , HE.onValueInput (HE.input (\v → right ∘ SetCustomPageSize v))
                ]
             else HH.select
                [ HE.onValueChange
                    (HE.input \v →
                      right ∘
                        if v ≡ "Custom"
                        then StartEnterCustomPageSize
                        else ChangePageSize v)
                ]
                pageOptions
           ]
    ]
  where

  sizeNum = fromMaybe 10 $ Int.fromString (fromInputValue pageSize)

  sizeValues = [10, 25, 50, 100]

  pageOptions = join
    [ option <$> sizeValues
    , dividerOption
    , customOption
    , [ HH.option_ [ HH.text "Custom" ] ]
    ]

  option value =
    HH.option
      [ HP.selected (value ≡ sizeNum) ]
      [ HH.text (show value) ]

  -- An unselectable option dividing the custom values from the presets
  dividerOption =
    [ HH.option
        [ HP.disabled true ]
        [ HH.text $ fromChar $ fromCharCode 8212 ]
    ]

  -- If a custom value has been entered, create an entry for it in the dropdown
  customOption =
    if isNothing (A.elemIndex sizeNum sizeValues)
    then [ HH.option [HP.selected true] [HH.text (show sizeNum)] ]
    else []
