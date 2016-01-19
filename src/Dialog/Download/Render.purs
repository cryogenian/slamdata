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

module Dialog.Download.Render (render) where

import Prelude

import Control.UI.Browser (encodeURIComponent)

import Data.Either (either, isLeft, isRight)
import Data.Functor (($>))
import Data.Lens (LensP(), (^.), (.~))
import Data.Maybe (isNothing, maybe, isJust)
import Data.Path.Pathy (printPath)

import Halogen
import Halogen.CustomProps as Cp
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Events.Forms as E
import Halogen.HTML.Properties as P
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import Config.Paths as Config
import Dialog.Download.Query
import Dialog.Download.State
import Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import Model.Resource (Resource(), isFile, resourcePath, isHidden)
import Model.Download as D
import Quasar.Aff (reqHeadersToJSON)
import Render.Common (fadeWhen)
import Render.CssClasses as Rc
import Render.Download as Rd

render :: State -> ComponentHTML Query
render state =
  modalDialog
  [ modalHeader "Download"
  , modalBody
    $ H.form [ Cp.nonSubmit
             , P.classes [ Rc.dialogDownload ]
             ]
    [ resField state
    , fldName state
    , chkCompress state
    , options state
    , message state
    ]
  , modalFooter [ btnCancel state
                , btnDownload state ]
  ]

resField :: State -> ComponentHTML Query
resField state =
  H.div [ P.classes [ B.formGroup, Rc.downloadSource, B.clearfix ] ]
  [ H.label_ [ H.span_ [ H.text "Source" ]
             , H.input [ P.classes [ B.formControl ]
                       , E.onValueInput (E.input SourceTyped)
                       , P.value resValue
                       ]
             , H.span [ P.classes [ B.inputGroupBtn ] ]
               [ H.button [ P.classes [ B.btn, B.btnDefault ]
                          , E.onClick (\_ -> E.stopPropagation $>
                                             action ToggleList
                                      )
                          ]
                 [ H.span [ P.classes [ B.caret ] ]  [ ] ]
               ]
             ]
  , H.ul [ P.classes $ [ B.listGroup, Rc.fileListGroup ]
           <> fadeWhen (not $ state ^. _showSourcesList)
         ]
    $ resItem <$> state ^. _sources
  ]
  where
  resValue :: String
  resValue = either id resourcePath $ state ^. _source

resItem :: Resource -> ComponentHTML Query
resItem res =
  H.button [ P.classes ([ B.listGroupItem ]
                        <> (if isHidden res
                            then [ Rc.itemHidden ]
                            else [ ]))
           , E.onClick (E.input_ (SourceClicked res))
           ]
  [ H.text (resourcePath res) ]


fldName :: State -> ComponentHTML Query
fldName state =
  H.div [ P.classes [ B.formGroup, Rc.downloadTarget ] ]
  [ H.label_ [ H.span_ [ H.text "Target name" ]
             , H.input [ P.classes [ B.formControl ]
                       , P.value tgtValue
                       , E.onValueInput (E.input TargetTyped)
                       ]
             , H.div [ P.classes [ Rc.downloadTargetBox ] ]
               [ H.span_ [ H.text tgtValue ]
               , H.span_ [ H.text ext ]
               ]
             ]
  ]
  where
  tgtValue :: String
  tgtValue = either id id $ state ^. _targetName

  ext | compressed state = ".zip"
      | isLeft (state ^. _options) = ".csv"
      | otherwise = ".json"

compressed :: State -> Boolean
compressed state =
 either (const false) (not <<< isFile) (state ^. _source) || state ^. _compress

chkCompress :: State -> ComponentHTML Query
chkCompress state =
  H.div [ P.classes [ B.formGroup ] ]
  [ H.label_ [ H.span_ [ H.text "Compress" ]
             , H.input [ P.type_ "checkbox"
                       , P.enabled $ either (const false) isFile
                         (state ^. _source)
                       , P.checked $ compressed state
                       , E.onValueChange (E.input_ ToggleCompress)
                       ]
             ]
  ]

options :: State -> ComponentHTML Query
options state =
  let opts = state ^. _options
      active = [ P.class_ B.active ]
  in H.div [ P.classes [ B.formGroup ] ]
     [ H.ul [ P.classes [ B.nav, B.navTabs ] ]
       [ H.li (if isLeft opts then active else [ ])
         [ H.a [ E.onClick (E.input_ $ SetOutput D.CSV)
               ]
           [ H.text "CSV" ]
         ]
       , H.li (if isRight opts then active else [ ])
         [ H.a [ E.onClick (E.input_ $ SetOutput D.JSON)
               ]
           [ H.text "JSON" ]
         ]
       ]
     , either optionsCSV optionsJSON opts
     ]

message :: State -> ComponentHTML Query
message state =
  let msg =  state ^. _error
  in H.div [ P.classes $ [ B.alert, B.alertDanger, B.alertDismissable ]
             <> fadeWhen (isNothing msg)
           ]
     $ maybe [ ] (pure <<< H.text) msg

btnCancel :: State -> ComponentHTML Query
btnCancel state =
  H.button [ P.classes [ B.btn ]
           , E.onClick (E.input_ Dismiss)
           , ARIA.label "Cancel download"
           , P.title "Cancel download"
           ]
  [ H.text "Cancel" ]

btnDownload :: State -> ComponentHTML Query
btnDownload state =
  let headers = encodeURIComponent $ show $ reqHeadersToJSON $ toHeaders state
      url = printPath Config.dataUrl
            <> either (const "#") resourcePath (state ^. _source)
            <> "?request-headers="
            <> headers
      disabled = isJust $ state ^. _error
  in H.a [ P.classes $ [ B.btn, B.btnPrimary ]
           <> if disabled
              then [ B.disabled ]
              else [ ]
         , P.disabled disabled
         , P.href url
         , E.onClick (\_ -> E.preventDefault $>
                            action (NewTab url)
                     )
         , ARIA.label "Proceed download"
         , P.title "Proceed download"
         ]
     [ H.text "Download" ]



optionsCSV :: D.CSVOptions -> ComponentHTML Query
optionsCSV = Rd.optionsCSV (\lens v -> ModifyCSVOpts (lens .~ v))

optionsJSON :: D.JSONOptions -> ComponentHTML Query
optionsJSON = Rd.optionsJSON (\lens v -> ModifyJSONOpts (lens .~ v))
