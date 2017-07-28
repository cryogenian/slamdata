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

module SlamData.FileSystem.Dialog.Mount.Couchbase.Component
  ( component
  , Query
  , module Q
  , module S
  ) where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Quasar.Mount.Couchbase as QMC
import SlamData.FileSystem.Dialog.Mount.Common.Render as MCR
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery as Q
import SlamData.FileSystem.Dialog.Mount.Common.State as MCS
import SlamData.FileSystem.Dialog.Mount.Couchbase.Component.State as S
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN

type Query = Q.SettingsQuery S.State
type Message = Q.SettingsMessage QMC.Config

type HTML = H.ComponentHTML Query

component ∷ H.Component HH.HTML Query (Maybe QMC.Config) Message Slam
component =
  H.component
    { initialState: maybe S.initialState S.fromConfig
    , render
    , eval: Q.eval (MCS.vToE ∘ S.toConfig)
    , receiver: const Nothing
    }

render ∷ S.State → HTML
render state =
  HH.div
    [ HP.class_ (H.ClassName "mount-couchbase") ]
    [ MCR.section "Server" [ MCR.host state S._host' ]
    , MCR.section "Bucket"
        [ HH.div
            [ HP.classes [CN.formGroup, CN.mountUserInfo] ]
            [ MCR.label "Bucket name" [ MCR.input state S._bucketName [] ]
            , MCR.label "Password" [ MCR.input state S._password [ HP.type_ HP.InputPassword ] ]
            , MCR.label "Document type" [ MCR.input state S._docTypeKey [] ]
            ]
        ]
    , MCR.section "Options"
        [ HH.div
            [ HP.class_ (HH.ClassName "sd-cb-options") ]
            [ HH.label_
                [ HH.span_ [ HH.text "Query timeout" ]
                , HH.input
                    [ HP.class_ CN.formControl
                    , HP.type_ HP.InputNumber
                    , HP.enabled (isJust state.queryTimeout)
                    , HP.value (fromMaybe "" state.queryTimeout)
                    , HP.min 1.0
                    ]
                , HH.span_ [ HH.text "(seconds)" ]
                ]
            , HH.label_
                [ HH.input
                    [ HP.type_ HP.InputCheckbox
                    , HP.checked (isJust state.queryTimeout)
                    , HE.onChecked $ HE.input_ $ Q.ModifyState \st →
                        st { queryTimeout = toggleTimeout st.queryTimeout }
                    ]
                , HH.span_ [ HH.text "Override default" ]
                ]
            ]
        ]
    ]
  where
    toggleTimeout = maybe (Just "30") (const Nothing)
