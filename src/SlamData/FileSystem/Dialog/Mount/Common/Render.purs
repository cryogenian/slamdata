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

module SlamData.FileSystem.Dialog.Mount.Common.Render where

import SlamData.Prelude

import Data.Array ((..))
import Data.Array as Arr
import Data.Lens (LensP, TraversalP, (.~), (^.))
import Data.Lens.Index (ix)
import Data.Profunctor.Strong (first, second)

import Halogen (HTML)
import Halogen.CustomProps as CP
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery (SettingsQuery(..))
import SlamData.Render.CSS as Rc

section :: forall p i. String -> Array (HTML p i) -> HTML p i
section title inner =
  H.div
    [ P.classes [ B.panel, B.panelDefault ] ]
    [ H.div
        [ P.class_ B.panelHeading ]
        [ H.text title ]
    , H.div
        [ P.class_ B.panelBody ]
        inner
    ]

propList
  :: forall s p
   . LensP s (Array (Tuple String String))
  -> s
  -> HTML p (SettingsQuery s)
propList _props state =
  H.div
    [ P.class_ Rc.mountProps ]
    [ H.table
        [ P.classes [B.table, B.tableBordered] ]
        [ H.thead_
            [ H.tr_
                [ H.th_ [ H.text "Name" ]
                , H.th_ [ H.text "Value" ]
                ]
            ]
        , H.tbody_
            [ H.tr_
                [ H.td
                    [ P.colSpan 2 ]
                    [ H.div
                        [ P.class_ Rc.mountPropsScrollbox ]
                        [ H.table_ propRows ]
                    ]
                ]
            ]
        ]
    ]
  where
  propRows = prop <$> 0 .. (Arr.length (state ^. _props) - 1)

  prop :: Int -> HTML p (SettingsQuery s)
  prop index =
    H.tr_ [ part first, part second ]
    where
    part :: LensP (Tuple String String) String -> HTML p (SettingsQuery s)
    part lens = H.td_ [ input (_props <<< ix index <<< lens) classes ]
    classes = [ P.classes [B.formControl, B.inputSm] ]

  input
    :: TraversalP s String
    -> Array (CP.InputProp (SettingsQuery s))
    -> HTML p (SettingsQuery s)
  input lens attrs =
    H.input
      $ [ P.class_ B.formControl
        , E.onValueInput (E.input \val -> ModifyState (lens .~ val))
        , P.value (state ^. lens)
        ]
      ++ attrs
