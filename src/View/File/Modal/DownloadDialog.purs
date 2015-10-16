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

module View.File.Modal.DownloadDialog (downloadDialog) where

import Prelude
import Api.Common (reqHeadersToJSON)
import Data.Functor (($>))
import Controller.File.Dialog.Download
import Utils (encodeURIComponent)
import Data.Either (either, isLeft, isRight)
import Data.Inject1 (inj)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe, fromMaybe)
import Data.Path.Pathy (printPath)
import Input.File (FileInput(..))
import Model.File (_dialog)
import Model.File.Dialog.Download
import Model.Resource (Resource(), resourcePath, isHidden, isFile)
import Optic.Core
import Optic.Refractor.Prism (_Left, _Right)
import View.Common (closeButton, fadeWhen)
import View.File.Common (HTML())
import View.Modal.Common (header, body, footer, h4, nonSubmit)

import qualified Config.Paths as Config
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as VC
import Utils.Halide (ariaLabel)

downloadDialog :: forall e. DownloadDialogRec -> Array (HTML e)
downloadDialog state =
  [ header $ h4 "Download"
  , body [ H.form [ A.class_ VC.dialogDownload
                  , nonSubmit
                  ]
                  [ resField
                  , fldName
                  , chkCompress
                  , options
                  , message (state ^. _error)
                  ]
         ]
  , footer [ btnCancel
           , btnDownload
           ]
  ]

  where

  resField :: HTML e
  resField =
    let value = either id resourcePath $ state ^. _source
    in H.div [ A.classes [B.formGroup, VC.downloadSource, B.clearfix] ]
             [ H.label_ [ H.span_ [ H.text "Source" ]
                        , H.input [ A.classes [ B.formControl ]
                                  , E.onInput (pure <<< handleSourceInput)
                                  , A.value value
                                  ]
                                  []
                        , H.span [ A.classes [B.inputGroupBtn] ]
                                 [ H.button [ A.classes [B.btn, B.btnDefault]
                                            , E.onClick (\_ -> E.stopPropagation $> handleToggleList)
                                            ]
                                            [ H.span [ A.classes [B.caret] ] [] ]
                                 ]
                        ]
             , H.ul [ A.classes $ [B.listGroup, VC.fileListGroup, B.fade] <> fadeWhen (not $ state ^. _showSourcesList) ]
                    $ resItem <$> state ^. _sources
             ]

  resItem :: Resource -> HTML e
  resItem res =
    H.button [ A.classes ([B.listGroupItem] <> (if isHidden res
                                                then [VC.itemHidden]
                                                else []))
             , E.onClick (\_ -> pure $ handleSourceClicked res)
             ]
             [ H.text (resourcePath res) ]

  fldName :: HTML e
  fldName =
    let value = either id id $ state ^. _targetName
        ext | compressed = ".zip"
            | isLeft (state ^. _options) = ".csv"
            | otherwise = ".json"
    in  H.div [ A.classes [B.formGroup, VC.downloadTarget] ]
              [ H.label_ [ H.span_ [ H.text "Target name" ]
                         , H.input [ A.classes [B.formControl]
                                   , A.value value
                                   , E.onInput (pure <<< handleTargetNameChange)
                                   ]
                                   []
                         , H.div [ A.class_ VC.downloadTargetBox ]
                                 [ H.span_ [ H.text value ]
                                 , H.span_ [ H.text ext ]
                                 ]
                         ]
              ]

  chkCompress :: HTML e
  chkCompress =
    H.div [ A.class_ B.formGroup ]
          [ H.label_ [ H.span_ [ H.text "Compress" ]
                     , H.input [ A.type_ "checkbox"
                               , A.enabled $ either (const false) isFile (state ^. _source)
                               , A.checked compressed
                               , E.onChange (\_ -> pure $ handleCompressToggle)
                               ]
                               []
                     ]
          ]

  compressed :: Boolean
  compressed = either (const false) (not <<< isFile) (state ^. _source) || state ^. _compress

  options :: HTML e
  options =
    let opts = state ^. _options
        active = [ A.class_ B.active ]
    in H.div [ A.class_ B.formGroup ]
             [ H.ul [ A.classes [B.nav, B.navTabs] ]
                    [ H.li (if isLeft opts then active else [])
                           [ H.a [ E.onClick (\_ -> pure $ handleChangeOutput CSV) ]
                                 [ H.text "CSV" ]
                           ]
                    , H.li (if isRight opts then active else [])
                           [ H.a [ E.onClick (\_ -> pure $ handleChangeOutput JSON) ]
                                 [ H.text "JSON" ]
                           ]
                    ]
             , either optionsCSV optionsJSON opts
             ]

  message :: forall e. Maybe String -> HTML e
  message msg =
    H.div [ A.classes $ [B.alert, B.alertDanger, B.alertDismissable] ++ fadeWhen (isNothing msg) ]
          $ maybe [] (pure <<< H.text) msg

  btnCancel :: forall e. HTML e
  btnCancel =
    H.button [ A.classes [B.btn]
             , E.onClick (E.input_ $ inj $ WithState (_dialog .~ Nothing))
             , ariaLabel "Cancel download"
             ]
             [ H.text "Cancel" ]

  btnDownload :: forall e. HTML e
  btnDownload =
    let headers = encodeURIComponent $ show $ reqHeadersToJSON $ toHeaders state
        url = printPath Config.dataUrl ++ either (const "#") resourcePath (state ^. _source) ++ "?request-headers=" ++ headers
        disabled = isJust $ state ^. _error
    in H.a [ A.classes $ [B.btn, B.btnPrimary] ++ if disabled then [B.disabled] else []
           , A.disabled disabled
           , A.href url
           , E.onClick (\ev -> E.preventDefault $> handleDownloadClick ev.target)
           , ariaLabel "Proceed download"
           ]
           [ H.text "Download" ]

optionsCSV :: forall e. CSVOptions -> HTML e
optionsCSV opts =
  H.div_ [ H.ul [ A.classes [VC.downloadCSVDelimiters, B.clearfix] ]
                [ field _rowDelimiter "Row delimiter"
                , field _colDelimiter "Column delimiter"
                , field _quoteChar "Quote character"
                , field _escapeChar "Quote escape"
                ]
         ]
  where

  field :: LensP CSVOptions String -> String -> HTML e
  field lens label =
    H.li_ [ H.label_ [ H.span_ [ H.text label ]
                     , H.input [ A.class_ B.formControl
                               , A.value (opts ^. lens)
                               , E.onInput (pure <<< handleOptionChange <<< set (_Left .. lens))
                               ] []
                     ]
          ]

  -- TODO: this is unused currently, awaiting a feature in Quasar
  arrays :: HTML e
  arrays =
    let sepVal = separateValue $ opts ^. _arrays
        isFlatMode = isNothing sepVal
    in H.div [ A.classes [B.clearfix, VC.downloadArrayMode] ]
             [ H.label_ [ H.text "Array flattening" ]
             , H.ul_ [ H.li_ [ H.label_ [ H.input [ A.type_ "radio"
                                                  , A.name "arraymode"
                                                  , A.checked isFlatMode
                                                  , E.onChange (\_ -> pure $ handleOptionChange $ _Left .. _arrays .~ Flatten)
                                                  ] []
                                        , H.text "Flatten into columns"
                                        ]
                             ]
                     , H.li_ [ H.label_ [ H.input [ A.type_ "radio"
                                                  , A.name "arraymode"
                                                  , A.checked (not isFlatMode)
                                                  , E.onChange (\_ -> pure $ handleOptionChange $ _Left .. _arrays .~ Separate "|")
                                                  ] []
                                        , H.text "Separate elements with: "
                                        , H.input [ A.class_ B.formControl
                                                  , A.enabled (not isFlatMode)
                                                  , A.value (fromMaybe "" sepVal)
                                                  , E.onInput (pure <<< handleOptionChange <<< set (_Left .. _arrays) <<< Separate)
                                                  ]
                                                  []
                                        ]
                             ]
                     ]
             ]

optionsJSON :: forall e. JSONOptions -> HTML e
optionsJSON opts = H.div [ A.class_ VC.downloadJSONOptions ]
                         [ multivalues, precision ]
  where

  multivalues :: HTML e
  multivalues =
    H.div [ A.class_ B.clearfix ]
          [ H.label_ [ H.text "Multiple values" ]
          , H.ul [ A.class_ B.clearfix ]
                 [ radio "multivalues" _multivalues ArrayWrapped "Wrap values in array"
                 , radio "multivalues" _multivalues LineDelimited "Separate values by newlines"
                 ]
          ]

  precision :: HTML e
  precision =
    H.div [ A.class_ B.clearfix ]
          [ H.label_ [ H.text "Precision" ]
          , H.ul_ [ radio "precision" _precision Readable "Readable"
                  , radio "precision" _precision Precise  "Encode all types"
                  ]
          ]

  radio :: forall a. (Eq a) => String -> LensP JSONOptions a -> a -> String -> HTML e
  radio grp lens value label =
     H.li_ [ H.label_ [ H.input [ A.type_ "radio"
                                , A.name grp
                                , A.checked (opts ^. lens == value)
                                , E.onChange (\_ -> pure $ handleOptionChange (_Right .. lens .~ value))
                                ] []
                      , H.text label
                      ]
           ]
