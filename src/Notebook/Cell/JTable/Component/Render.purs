{-
Copyright 2015 SlamData, Inc.

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

module Notebook.Cell.JTable.Component.Render (render) where

import Prelude

import Control.Alt ((<|>))
import Control.Bind ((=<<), join)

import Data.Array as A
import Data.Char (fromCharCode)
import Data.Either (Either(), either)
import Data.Functor (($>))
import Data.Functor.Coproduct (left, right)
import Data.Int as Int
import Data.Json.JTable as JT
import Data.Maybe (Maybe(..), isNothing, fromMaybe)
import Data.String (fromChar)
import Data.These (These(), theseLeft, theseRight)

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Events.Handler as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import Render.Common (glyph)
import Render.CssClasses as CSS

import Notebook.Cell.Common.EvalQuery (CellEvalQuery(..))
import Notebook.Cell.JTable.Component.Query
import Notebook.Cell.JTable.Component.State

render :: JTableState -> ComponentHTML JTableQueryP
render { json: Nothing } = H.div_ []
render st@{ json: Just json } =
  let p = currentPageInfo st
  in H.div_
    [ right <$> JT.renderJTable jTableOpts json
    , H.div
        [ P.class_ CSS.pagination ]
        [ prevButtons (p.page <= 1)
        , pageField st.page p.totalPages
        , nextButtons (p.page >= p.totalPages)
        , pageSizeControls st.isEnteringPageSize st.pageSize
        ]
    ]

jTableOpts :: JT.JTableOpts
jTableOpts = JT.jTableOptsDefault
  { style = JT.bootstrapStyle
  , columnOrdering = JT.alphaOrdering
  }

prevButtons :: Boolean -> ComponentHTML JTableQueryP
prevButtons enabled =
  H.div
    [ P.classes [B.btnGroup] ]
    [ H.button
        [ P.classes [B.btn, B.btnSm, B.btnDefault]
        , P.disabled enabled
        , E.onClick $ E.input_ (right <<< StepPage First)
        ]
        [ glyph B.glyphiconFastBackward ]
    , H.button
        [ P.classes [B.btn, B.btnSm, B.btnDefault]
        , P.disabled enabled
        , E.onClick $ E.input_ (right <<< StepPage Prev)
        ]
        [ glyph B.glyphiconStepBackward ]
    ]

pageField :: These (Either String Int) Int -> Int -> ComponentHTML JTableQueryP
pageField pageValue totalPages =
  H.div
    [ P.classes [CSS.pageInput] ]
    [ submittable
        [ H.text "Page"
        , H.input
            [ P.classes [B.formControl, B.inputSm]
            , P.value $ fromMaybe "1" (theseToValue pageValue)
            , E.onValueInput (E.input (\x -> right <<< SetCustomPage x))
            ]
        , H.text $ "of " ++ (show totalPages)
        ]
    ]

submittable :: Array (ComponentHTML JTableQueryP) -> ComponentHTML JTableQueryP
submittable =
  H.form
    [ E.onSubmit (\_ -> E.preventDefault $> action (left <<< NotifyRunCell)) ]

nextButtons :: Boolean -> ComponentHTML JTableQueryP
nextButtons enabled =
  H.div
    [ P.classes [B.btnGroup] ]
    [ H.button
        [ P.classes [B.btn, B.btnSm, B.btnDefault]
        , P.disabled enabled
        , E.onClick $ E.input_ (right <<< StepPage Next)
        ]
        [ glyph B.glyphiconStepForward ]
    , H.button
        [ P.classes [B.btn, B.btnSm, B.btnDefault]
        , P.disabled enabled
        , E.onClick $ E.input_ (right <<< StepPage Last)
        ]
        [ glyph B.glyphiconFastForward ]
    ]

pageSizeControls :: Boolean -> These (Either String Int) Int -> ComponentHTML JTableQueryP
pageSizeControls showCustom pageSize =
  H.div
    [ P.classes [CSS.pageSize] ]
    [ submittable
         $ [ H.text "Per page:" ]
        ++ [ if showCustom
             then H.input
                [ P.classes [B.formControl, B.inputSm]
                , P.value $ fromMaybe "10" (theseToValue pageSize)
                , E.onValueInput (E.input (\v -> right <<< SetCustomPageSize v))
                ]
             else H.select
                [ P.classes [B.formControl, B.inputSm]
                , E.onValueChange
                    (E.input (\v ->
                      right <<<
                        if v == "Custom"
                        then StartEnterCustomPageSize
                        else ChangePageSize v))
                ]
                pageOptions
           ]
    ]
  where

  sizeNum = fromMaybe 10 (Int.fromString =<< theseToValue pageSize)

  sizeValues = [10, 25, 50, 100]

  pageOptions = join
    [ option <$> sizeValues
    , dividerOption
    , customOption
    , [ H.option_ [ H.text "Custom" ] ]
    ]

  option value =
    H.option
      [ P.selected (value == sizeNum) ]
      [ H.text (show value) ]

  -- An unselectable option dividing the custom values from the presets
  dividerOption =
    [ H.option [ P.disabled true ] [ H.text $ fromChar $ fromCharCode 8212 ] ]

  -- If a custom value has been entered, create an entry for it in the dropdown
  customOption =
    if isNothing (A.elemIndex sizeNum sizeValues)
    then [ H.option [P.selected true] [H.text (show sizeNum)] ]
    else []

theseToValue :: These (Either String Int) Int -> Maybe String
theseToValue x =
  let customValue = either id show <$> theseLeft x
      actualValue = show <$> theseRight x
  in customValue <|> actualValue
