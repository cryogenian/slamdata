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

import Quasar.Paths as Config

import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import SlamData.Download.Model as D
import SlamData.Download.Render as Rd
import SlamData.FileSystem.Dialog.Download.Component.Query (Query(..))
import SlamData.FileSystem.Dialog.Download.Component.State (State)
import SlamData.FileSystem.Resource (Resource, isFile, resourcePath, isHidden)
import SlamData.Quasar (reqHeadersToJSON, encodeURI)
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
      , fldName state
      , chkCompress state
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
        , HH.input
            [ HP.classes [ B.formControl ]
            , HE.onValueInput (HE.input SourceTyped)
            , HP.value resValue
            ]
        , HH.span
            [ HP.classes [ B.inputGroupBtn ] ]
            [ HH.button
                [ HP.classes [ B.btn, B.btnDefault ]
                , HE.onClick \_ → HEH.stopPropagation $> H.action ToggleList
                ]
                [ HH.span [ HP.classes [ B.caret ] ]  [ ] ]
            ]
        ]
    , HH.ul
        [ HP.classes
            $ [ B.listGroup, Rc.fileListGroup ]
            ⊕ fadeWhen (not $ state.showSourcesList)
        ]
        $ resItem <$> state.sources
    ]
  where
  resValue ∷ String
  resValue = either id resourcePath $ state.source

resItem ∷ Resource → H.ComponentHTML Query
resItem res =
  HH.button
    [ HP.classes
        $ [ B.listGroupItem ]
        ⊕ if isHidden res then [ Rc.itemHidden ] else []
    , HE.onClick (HE.input_ (SourceClicked res))
    ]
    [ HH.text (resourcePath res) ]


fldName ∷ State → H.ComponentHTML Query
fldName state =
  HH.div
    [ HP.classes [ B.formGroup, Rc.downloadTarget ] ]
    [ HH.label_
        [ HH.span_ [ HH.text "Target name" ]
        , HH.input
            [ HP.classes [ B.formControl ]
            , HP.value tgtValue
            , HE.onValueInput (HE.input TargetTyped)
            ]
        , HH.div
            [ HP.classes [ Rc.downloadTargetBox ] ]
            [ HH.span_ [ HH.text tgtValue ]
            , HH.span_ [ HH.text ext ]
            ]
        ]
    ]
  where
  tgtValue ∷ String
  tgtValue = either id id $ state.targetName

  ext | compressed state = ".zip"
      | isLeft (state.options) = ".csv"
      | otherwise = ".json"

compressed ∷ State → Boolean
compressed state =
 either (const false) (not ∘ isFile) (state.source) || state.compress

chkCompress ∷ State → H.ComponentHTML Query
chkCompress state =
  HH.div [ HP.classes [ B.formGroup ] ]
  [ HH.label_ [ HH.span_ [ HH.text "Compress" ]
             , HH.input [ HP.inputType HP.InputCheckbox
                       , HP.enabled $ either (const false) isFile
                         (state.source)
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
  let headers = Global.encodeURIComponent $ show $ reqHeadersToJSON $ D.toHeaders state
      url = (encodeURI
             $ printPath Config.data_
             ⊕ either (const "#") resourcePath (state.source))
            ⊕ "?request-headers="
            ⊕ headers
      disabled = isJust $ state.error
  in HH.button
         [ HP.classes $ [ B.btn, B.btnPrimary ]
           ⊕ if disabled
              then [ B.disabled ]
              else [ ]
         , HP.disabled disabled
         , HE.onClick (\_ → HEH.preventDefault $> H.action (NewTab url)
                     )
         , ARIA.label "Proceed download"
         , HP.title "Proceed download"
         ]
     [ HH.text "Download" ]



optionsCSV ∷ D.CSVOptions → H.ComponentHTML Query
optionsCSV = Rd.optionsCSV (\lens v → ModifyCSVOpts (lens .~ v))

optionsJSON ∷ D.JSONOptions → H.ComponentHTML Query
optionsJSON = Rd.optionsJSON (\lens v → ModifyJSONOpts (lens .~ v))
