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
  , module SlamData.FileSystem.Dialog.Mount.MongoDB.Component.State
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Exception (error)

import Data.Array ((..), length)
import Data.Lens (TraversalP, (^.), (.~))
import Data.Lens.Index (ix)
import Data.Path.Pathy (dir, (</>))
import Data.String.Regex as Rx

import Halogen as H
import Halogen.CustomProps as Cp
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.FileSystem.Dialog.Mount.Common.Render (propList, section)
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery (SettingsQuery(..))
import SlamData.FileSystem.Dialog.Mount.MongoDB.Component.State (MountHost, MountProp, State, _host, _hosts, _password, _path, _port, _props, _user, initialState, processState, fromConfig, toConfig)
import SlamData.FileSystem.Resource (Mount(..))
import SlamData.Quasar.Mount as API
import SlamData.Render.CSS as Rc

type Query = SettingsQuery State

type HTML = H.ComponentHTML Query

comp :: H.Component State Query Slam
comp = H.component { render, eval }

render :: State -> HTML
render state =
  HH.div
    [ HP.key "mount-mongodb"
    , HP.class_ Rc.mountMongoDB
    ]
    [ section "Server(s)" [ hosts state ]
    , section "Authentication" [ userInfo state, fldPath state ]
    , section "Settings" [ propList _props state ]
    ]

eval :: Natural Query (H.ComponentDSL State Query Slam)
eval (ModifyState f next) = H.modify (processState <<< f) $> next
eval (Validate k) =
  k <<< either Just (const Nothing) <<< toConfig <$> H.get

eval (Submit parent name k) = do
  st <- H.get
  case toConfig st of
    Left err ->
      pure $ k $ Left $ error err
    Right config → do
      let path = parent </> dir name
      result <- API.saveMount path config
      pure $ k $ map (const (Database path)) result

hosts :: State -> H.ComponentHTML Query
hosts state =
  HH.div
    [ HP.class_ Rc.mountHostList ]
    $ host state <$> 0 .. (length state.hosts - 1)

host :: State -> Int -> H.ComponentHTML Query
host state index =
  HH.div
    [ HP.class_ Rc.mountHost ]
    [ label "Host"
        [ input' rejectNonHostname state (_hosts <<< ix index <<< _host) [] ]
    , label "Port"
        [ input' rejectNonPort state (_hosts <<< ix index <<< _port) [] ]
    ]
  where
  rejectNonHostname :: String -> String
  rejectNonHostname = Rx.replace rxNonHostname ""

  rxNonHostname :: Rx.Regex
  rxNonHostname =
    Rx.regex "[^0-9a-z\\-\\._~%]" (Rx.noFlags { ignoreCase = true, global = true })

  rejectNonPort :: String -> String
  rejectNonPort = Rx.replace rxNonPort ""

  rxNonPort :: Rx.Regex
  rxNonPort = Rx.regex "[^0-9]" (Rx.noFlags { global = true })

userInfo :: State -> H.ComponentHTML Query
userInfo state =
  HH.div
    [ HP.classes [B.formGroup, Rc.mountUserInfo] ]
    [ fldUser state, fldPass state ]

fldUser :: State -> H.ComponentHTML Query
fldUser state =
  label "Username" [ input state _user [] ]

fldPass :: State -> H.ComponentHTML Query
fldPass state =
  label "Password" [ input state _password [ HP.inputType HP.InputPassword ] ]

fldPath :: State -> H.ComponentHTML Query
fldPath state =
  HH.div
    [ HP.class_ Rc.mountPath ]
    [ label "Database" [ input state _path [] ] ]

-- | A labelled section within the form.
label :: String -> Array HTML -> HTML
label text inner = HH.label_ $ [ HH.span_ [ HH.text text ] ] ++ inner

-- | A basic text input field that uses a lens to read from and update the
-- | state.
input
  :: State
  -> TraversalP State String
  -> Array (Cp.InputProp Query)
  -> HTML
input state lens =
  input' id state lens -- can't eta reduce further here as the typechecker doesn't like it

-- | A basic text input field that uses a lens to read from and update the
-- | state, and allows for the input value to be modified.
input'
  :: (String -> String)
  -> State
  -> TraversalP State String
  -> Array (Cp.InputProp Query)
  -> HTML
input' f state lens attrs =
  HH.input
    $ [ HP.class_ B.formControl
      , HE.onValueInput (HE.input \val -> ModifyState (lens .~ f val))
      , HP.value (state ^. lens)
      ]
    ++ attrs
