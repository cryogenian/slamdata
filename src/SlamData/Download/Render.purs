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

module SlamData.Download.Render where

import Prelude

import Data.Lens (LensP(), (^.))

import Halogen
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import SlamData.Download.Model as D
import SlamData.Render.CSS as Rc

optionsCSV
  :: forall f
   . (LensP D.CSVOptions String -> String -> (Unit -> f Unit))
   -> D.CSVOptions
   -> ComponentHTML f
optionsCSV func opts =
  H.div_ [ H.ul [ P.classes [ Rc.downloadCSVDelimiters, B.clearfix ]]
           [ field D._rowDelimiter "Row delimiter"
           , field D._colDelimiter "Column delimiter"
           , field D._quoteChar "Quote character"
           , field D._escapeChar "Quote escape"
             ]
         ]
  where
  field :: LensP D.CSVOptions String -> String -> ComponentHTML f
  field lens label =
    H.li_ [ H.label_ [ H.span_ [ H.text label ]
                     , H.input [ P.classes [ B.formControl ]
                               , P.value (opts ^. lens)
                               , E.onValueInput (\v -> pure $ func lens v unit)
                               ]
                     ]
          ]

optionsJSON
  :: forall f
   . (forall a. (Eq a) => LensP D.JSONOptions a -> a -> (Unit -> f Unit))
  -> D.JSONOptions
  -> ComponentHTML f
optionsJSON func opts =
  H.div [ P.classes [ Rc.downloadJSONOptions ] ]
  [ multivalues, precision ]
  where
  multivalues :: ComponentHTML f
  multivalues =
    H.div [ P.classes [ B.clearfix ] ]
    [ H.label_ [ H.text "Multiple values" ]
    , H.ul_
      [ radio "multivalues" D._multivalues D.ArrayWrapped "Wrap values in arrays"
      , radio "multivalues" D._multivalues D.LineDelimited "Separate values by newlines"
      ]
    ]

  precision :: ComponentHTML f
  precision =
    H.div [ P.classes [ B.clearfix ] ]
    [ H.label_ [ H.text "Precision" ]
    , H.ul_ [ radio "precision" D._precision D.Readable "Readable"
            , radio "precision" D._precision D.Precise "Encode all types"
            ]
    ]

  radio
    :: forall a
     . (Eq a)
    => String -> LensP D.JSONOptions a -> a -> String -> ComponentHTML f
  radio grp lens value label =
    H.li_ [ H.label_ [ H.input [ P.inputType P.InputRadio
                               , P.name grp
                               , P.checked (opts ^. lens == value)
                               , E.onValueChange (E.input_ (func lens value))
                               ]
                     , H.text label
                     ]
          ]
