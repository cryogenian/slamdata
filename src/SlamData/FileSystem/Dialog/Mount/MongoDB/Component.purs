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

module SlamData.FileSystem.Dialog.Mount.MongoDB.Component
  ( comp
  , Query
  , module SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery
  , module MCS
  ) where

import SlamData.Prelude

import Data.Path.Pathy (dir, (</>))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as B

import Quasar.Mount as QM

import SlamData.FileSystem.Dialog.Mount.Common.Render as MCR
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery (SettingsQuery(..), SettingsMessage(..))
import SlamData.FileSystem.Dialog.Mount.MongoDB.Component.State as MCS
import SlamData.FileSystem.Resource (Mount(..))
import SlamData.Monad (Slam)
import SlamData.Quasar.Error as QE
import SlamData.Quasar.Mount as API
import SlamData.Render.CSS as Rc

type Query = SettingsQuery MCS.State

comp ∷ H.Component HH.HTML Query Unit SettingsMessage Slam
comp =
  H.component
    { initialState: const MCS.initialState
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ MCS.State → H.ComponentHTML Query
render state =
  HH.div
    [ HP.class_ Rc.mountMongoDB ]
    [ MCR.section "Server(s)" [ MCR.hosts state MCS._hosts ]
    , MCR.section "Authentication"
        [ HH.div
            [ HP.classes [B.formGroup, Rc.mountUserInfo] ]
            [ MCR.label "Username" [ MCR.input state MCS._user [] ]
            , MCR.label "Password" [ MCR.input state MCS._password [ HP.type_ HP.InputPassword ] ]
            ]
        , HH.div
            [ HP.class_ Rc.mountPath ]
            [ MCR.label "Database" [ MCR.input state MCS._path [] ] ]
        ]
    , MCR.section "Settings" [ MCR.propList MCS._props state ]
    ]

eval ∷ Query ~> H.ComponentDSL MCS.State Query SettingsMessage Slam
eval = case _ of
  ModifyState f next → do
    H.modify (MCS.processState <<< f)
    H.raise Modified
    pure next
  Validate k →
    k ∘ either Just (const Nothing) ∘ MCS.toConfig <$> H.get
  Submit parent name k →
    k <$> runExceptT do
      let
        path = parent </> dir name
      st ← lift H.get
      config ← except $ lmap QE.msgToQError $ MCS.toConfig st
      ExceptT $ API.saveMount (Left path) (QM.MongoDBConfig config)
      pure $ Database path
