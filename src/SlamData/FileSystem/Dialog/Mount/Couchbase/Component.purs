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
  ( comp
  , Query
  , module SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery
  , module S
  ) where

import SlamData.Prelude

import Data.Path.Pathy (dir, (</>))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Quasar.Mount as QM
import SlamData.FileSystem.Dialog.Mount.Common.Render as MCR
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery (SettingsQuery(..), SettingsMessage(..))
import SlamData.FileSystem.Dialog.Mount.Common.State as MCS
import SlamData.FileSystem.Dialog.Mount.Couchbase.Component.State as S
import SlamData.FileSystem.Resource (Mount(..))
import SlamData.Monad (Slam)
import SlamData.Quasar.Error as QE
import SlamData.Quasar.Mount as API
import SlamData.Render.ClassName as CN

type Query = SettingsQuery S.State

type HTML = H.ComponentHTML Query

comp ∷ S.State → H.Component HH.HTML Query Unit SettingsMessage Slam
comp initialState =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ S.State → HTML
render state =
  HH.div
    [ HP.class_ CN.mountCouchbase ]
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
                    , HE.onChecked $ HE.input_ $ ModifyState \st →
                        st { queryTimeout = toggleTimeout st.queryTimeout }
                    ]
                , HH.span_ [ HH.text "Override default" ]
                ]
            ]
        ]
    ]
  where
    toggleTimeout = maybe (Just "30") (const Nothing)

eval ∷ Query ~> H.ComponentDSL S.State Query SettingsMessage Slam
eval = case _ of
  ModifyState f next → do
    H.modify f
    H.raise Modified
    pure next
  Validate k →
    k ∘ either Just (const Nothing) ∘ MCS.vToE ∘ S.toConfig <$> H.get
  Submit parent name k →
    k <$> runExceptT do
      st ← lift H.get
      config ← except $ lmap QE.msgToQError $ MCS.vToE $ S.toConfig st
      let path = parent </> dir name
      ExceptT $ API.saveMount (Left path) (QM.CouchbaseConfig config)
      pure $ Database path
