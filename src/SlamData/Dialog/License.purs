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

module SlamData.Dialog.License where

import SlamData.Prelude

import Halogen.HTML as H
import Halogen.HTML.Properties as HP

import SlamData.Render.ClassName as CN
import SlamData.Dialog.Render as DR

data LaunchedBy = App | Other

advancedLicenseExpired ∷ ∀ f p. H.HTML p (f Unit)
advancedLicenseExpired =
  H.div
    [ HP.classes [ H.ClassName "deck-dialog", H.ClassName "license-dialog" ] ]
    [ H.div_
        [ H.img [ HP.src "img/features.png" ]
        , DR.modalHeader "Your license has expired"
        , DR.modalBody
            $ H.div_
                [ H.p_
                    [ H.text "Thanks for using SlamData Advanced!" ]
                , H.p_
                    [ H.text "Get in touch with us today to purchase SlamData Advanced or an extended trial period with raining for your team, configuration and optimization assistance and support ith queries, sharing and distribution." ]
              ]
        , DR.modalFooter
            [ H.a
                [ HP.classes [ CN.btn, CN.btnPrimary ]
                , HP.href "https://slamdata.com/contact-us/"
                ]
                [ H.text "Contact SlamData" ]
            , H.a
                [ HP.classes [ CN.btn, CN.btnDefault ]
                , HP.href "https://slamdata.com/slamdata-jump-start/"
                ]
                [ H.text "Request training" ]
            ]
        ]
  ]

advancedTrialLicenseExpired ∷ ∀ f p. H.HTML p (f Unit)
advancedTrialLicenseExpired =
  H.div
    [ HP.classes [ H.ClassName "deck-dialog", H.ClassName "license-dialog" ] ]
    [ H.div_
        [ H.img [ HP.src "img/features.png" ]
        , DR.modalHeader "Your trial has expired"
        , DR.modalBody
            $ H.div_
                [ H.p_
                    [ H.text "Thanks for trying SlamData Advanced!" ]
                , H.p_
                    [ H.text "Get in touch with us today to purchase a SlamData license or an extended trial period with training for your team, configuration, query optimization and support." ]
              ]
        , DR.modalFooter
            [ H.a
                [ HP.classes [ CN.btn, CN.btnPrimary ]
                , HP.href "https://slamdata.com/contact-us/"
                ]
                [ H.text "Contact SlamData" ]
            , H.a
                [ HP.classes [ CN.btn, CN.btnDefault ]
                , HP.href "https://slamdata.com/slamdata-jump-start/"
                ]
                [ H.text "Request training" ]
            ]
        ]
  ]

licenseInvalid ∷ ∀ f p. H.HTML p (f Unit)
licenseInvalid =
  H.div
    [ HP.classes [ H.ClassName "deck-dialog", H.ClassName "license-dialog" ] ]
    [ H.div_
        [ H.img [ HP.src "img/features.png" ]
        , DR.modalHeader "Your license is invalid"
        , DR.modalBody
            $ H.div_
                [ H.p_
                    [ H.text
                        $ "Try double checking your license information and "
                        <> howToFix App
                        <> " before restarting SlamData. If you are still experiencing problems with your license after trying this please contact support." ]
              ]
        , DR.modalFooter
            [ H.a
                [ HP.classes [ CN.btn, CN.btnPrimary ]
                , HP.href "https://slamdata.com/contact-us/"
                ]
                [ H.text "Contact SlamData" ]
            , H.a
                [ HP.classes [ CN.btn, CN.btnDefault ]
                , HP.href "https://slamdata.com/slamdata-jump-start/"
                ]
                [ H.text "Request training" ]
            ]
        ]
    ]
  where
  howToFix ∷ LaunchedBy → String
  howToFix = case _ of
    App → "reinstalling"
    Other → "providing your Java system properties"

