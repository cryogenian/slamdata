{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Dialog.License.Component where

import SlamData.Prelude

import DOM.Event.Types (MouseEvent)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Quasar.Advanced.Types as QAT
import SlamData.Dialog.Render as DR
import SlamData.License as License
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN
import Utils.DOM as DOM

data LaunchedBy = App | Other

data Query a
  = SetState (Maybe License.LicenseProblem) a
  | Dismiss MouseEvent a

data Message = Dismissed

type HTML = H.ComponentHTML Query

component ∷ H.Component HH.HTML Query (Maybe License.LicenseProblem) Message Slam
component =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: HE.input SetState
    }

render ∷ Maybe License.LicenseProblem → HTML
render = case _ of
  Nothing → HH.text ""
  Just problem →
    HH.div
      [ HP.class_ CN.dialogContainer
      , HE.onClick (HE.input Dismiss)
      ]
      [ case problem of
          License.Expired QAT.Advanced → advancedLicenseExpired
          License.Expired QAT.AdvancedTrial → advancedTrialLicenseExpired
          License.Invalid → licenseInvalid
      ]

licenseDialogClasses ∷ Array H.ClassName
licenseDialogClasses =
  [ CN.dialog, H.ClassName "license-dialog" ]

advancedLicenseExpired ∷ HTML
advancedLicenseExpired =
  HH.div
    [ HP.classes licenseDialogClasses ]
    [ HH.img [ HP.src "img/logo-center.svg" ]
    , DR.modalHeader "Your license has expired"
    , DR.modalBody
        $ HH.div_
            [ HH.p_
                [ HH.text "Thanks for using SlamData Advanced!" ]
            , HH.p_
                [ HH.text "Get in touch with us today to purchase SlamData Advanced or an extended trial period with raining for your team, configuration and optimization assistance and support ith queries, sharing and distribution." ]
          ]
    , DR.modalFooter
        [ HH.a
            [ HP.classes [ CN.btn, CN.btnPrimary ]
            , HP.href "https://slamdata.com/contact-us/"
            ]
            [ HH.text "Contact SlamData" ]
        , HH.a
            [ HP.classes [ CN.btn, CN.btnDefault ]
            , HP.href "https://slamdata.com/slamdata-jump-start/"
            ]
            [ HH.text "Request training" ]
        ]
  ]

advancedTrialLicenseExpired ∷ HTML
advancedTrialLicenseExpired =
  HH.div
    [ HP.classes licenseDialogClasses ]
    [ HH.div_
        [ HH.img [ HP.src "img/logo-center.svg" ]
        , DR.modalHeader "Your trial has expired"
        , DR.modalBody
            $ HH.div_
                [ HH.p_
                    [ HH.text "Thanks for trying SlamData Advanced!" ]
                , HH.p_
                    [ HH.text "Get in touch with us today to purchase a SlamData license or an extended trial period with training for your team, configuration, query optimization and support." ]
              ]
        , DR.modalFooter
            [ HH.a
                [ HP.classes [ CN.btn, CN.btnPrimary ]
                , HP.href "https://slamdata.com/contact-us/"
                ]
                [ HH.text "Contact SlamData" ]
            , HH.a
                [ HP.classes [ CN.btn, CN.btnDefault ]
                , HP.href "https://slamdata.com/slamdata-jump-start/"
                ]
                [ HH.text "Request training" ]
            ]
        ]
  ]

licenseInvalid ∷ HTML
licenseInvalid =
  HH.div
    [ HP.classes licenseDialogClasses ]
    [ HH.div_
        [ HH.img [ HP.src "img/logo-center.svg" ]
        , DR.modalHeader "Your license is invalid"
        , DR.modalBody
            $ HH.div_
                [ HH.p_
                    [ HH.text
                        $ "Try double checking your license information and "
                        <> howToFix App
                        <> " before restarting SlamData. If you are still experiencing problems with your license after trying this please contact support." ]
              ]
        , DR.modalFooter
            [ HH.a
                [ HP.classes [ CN.btn, CN.btnPrimary ]
                , HP.href "https://slamdata.com/contact-us/"
                ]
                [ HH.text "Contact SlamData" ]
            , HH.a
                [ HP.classes [ CN.btn, CN.btnDefault ]
                , HP.href "https://slamdata.com/slamdata-jump-start/"
                ]
                [ HH.text "Request training" ]
            ]
        ]
    ]
  where
  howToFix ∷ LaunchedBy → String
  howToFix = case _ of
    App → "reinstalling"
    Other → "providing your Java system properties"

eval ∷ Query ~> H.ComponentDSL (Maybe License.LicenseProblem) Query Message Slam
eval = case _ of
  SetState d next → do
    H.put d
    pure next
  Dismiss me next → do
    isBackdrop ← H.liftEff $ DOM.nodeEq (DOM.target me) (DOM.currentTarget me)
    when isBackdrop (H.put Nothing)
    pure next
