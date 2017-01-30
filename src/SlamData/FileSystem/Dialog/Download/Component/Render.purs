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

module SlamData.FileSystem.Dialog.Download.Component.Render (render) where

import SlamData.Prelude

import Data.Lens ((.~))
import Data.Path.Pathy (printPath)

import Halogen as H
import Halogen.CustomProps as Cp
import Halogen.HTML.Events.Handler as HEH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import Global as Global

import Quasar.Paths as Config

import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import SlamData.Download.Model as D
import SlamData.Download.Render as Rd
import SlamData.FileSystem.Dialog.Download.Component.Query (Query(..))
import SlamData.FileSystem.Dialog.Download.Component.State (State)
import SlamData.Quasar (reqHeadersToJSON, encodeURI)
import SlamData.FileSystem.Resource (resourcePath, isFile)
import SlamData.Render.Common (fadeWhen)
import SlamData.Render.CSS as Rc

render ∷ State → H.ComponentHTML Query
render state =
  modalDialog
  [ modalHeader "Download"
  , modalBody
      $ HH.form
          [ Cp.nonSubmit
          , HP.classes [ Rc.dialogDownload ]
          ]
      [ resField state
      , Rd.fldName state.options (either id id state.targetName) TargetTyped
      , options state
      , message state
      ]
  , modalFooter
      [ btnCancel state
      , btnDownload state
      ]
  ]

resField ∷ State → H.ComponentHTML Query
resField state =
  HH.div
    [ HP.classes [ B.formGroup, Rc.downloadSource, B.clearfix ] ]
    [ HH.label_
        [ HH.span_ [ HH.text "Source" ]
        , HH.text (resourcePath state.source)
        ]
    ]

compressed ∷ State → Boolean
compressed state = not isFile state.source || state.compress

chkCompress ∷ State → H.ComponentHTML Query
chkCompress state =
  HH.div
    [ HP.classes [ B.formGroup ] ]
    [ HH.label_
      [ HH.span_ [ HH.text "Compress" ]
      , HH.input
          [ HP.inputType HP.InputCheckbox
          , HP.enabled $ isFile state.source
          , HP.checked $ compressed state
          , HE.onValueChange (HE.input_ ToggleCompress)
          ]
      ]
    ]

options ∷ State → H.ComponentHTML Query
options state =
  let opts = state.options
      active = [ HP.class_ B.active ]
  in HH.div [ HP.classes [ B.formGroup ] ]
     [ HH.ul [ HP.classes [ B.nav, B.navTabs ] ]
       [ HH.li (if isLeft opts then active else [ ])
         [ HH.a [ HE.onClick (HE.input_ $ SetOutput D.CSV)
               ]
           [ HH.text "CSV" ]
         ]
       , HH.li (if isRight opts then active else [ ])
         [ HH.a [ HE.onClick (HE.input_ $ SetOutput D.JSON)
               ]
           [ HH.text "JSON" ]
         ]
       ]
     , either optionsCSV optionsJSON opts
     ]

message ∷ State → H.ComponentHTML Query
message state =
  let msg = state.error
  in HH.div [ HP.classes $ [ B.alert, B.alertDanger, B.alertDismissable ]
             ⊕ fadeWhen (isNothing msg)
           ]
     $ maybe [ ] (pure ∘ HH.text) msg

btnCancel ∷ State → H.ComponentHTML Query
btnCancel state =
  HH.button [ HP.classes [ B.btn ]
           , HE.onClick (HE.input_ Dismiss)
           , ARIA.label "Cancel download"
           , HP.title "Cancel download"
           ]
  [ HH.text "Cancel" ]

btnDownload ∷ State → H.ComponentHTML Query
btnDownload state =
  let
    headers =
      Global.encodeURIComponent
      $ show
      $ reqHeadersToJSON
      $ append state.authHeaders
      $ D.toHeaders state
      $ Just
      $ either id id state.targetName <> D.extension false state.options

    url =
      (encodeURI $ printPath Config.data_ ⊕ resourcePath state.source)
      ⊕ "?request-headers="
      ⊕ headers

    disabled = isJust $ state.error
  in HH.button
         [ HP.classes $ [ B.btn, B.btnPrimary ]
           ⊕ if disabled
              then [ B.disabled ]
              else [ ]
         , HP.disabled disabled
         , HE.onClick \_ →
             HEH.preventDefault $> Just (H.action (NewTab url))
         , ARIA.label "Proceed download"
         , HP.title "Proceed download"
         ]
     [ HH.text "Download" ]

optionsCSV ∷ D.CSVOptions → H.ComponentHTML Query
optionsCSV = Rd.optionsCSV (\lens v → ModifyCSVOpts (lens .~ v))

optionsJSON ∷ D.JSONOptions → H.ComponentHTML Query
optionsJSON = Rd.optionsJSON (\lens v → ModifyJSONOpts (lens .~ v))
