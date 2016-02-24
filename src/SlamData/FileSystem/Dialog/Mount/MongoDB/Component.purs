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
  , Query()
  , module SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery
  , module SlamData.FileSystem.Dialog.Mount.MongoDB.Component.State
  ) where

import Prelude

import Control.Monad.Aff (attempt)

import Data.Array ((..), length, null, filter)
import Data.Functor (($>))
import Data.Functor.Aff (liftAff)
import Data.Lens (TraversalP(), (^.), (.~))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..))
import Data.Path.Pathy (dir, (</>))
import Data.String.Regex as Rx

import Halogen
import Halogen.CustomProps as Cp
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import Quasar.Aff as API
import Quasar.Auth as Auth

import SlamData.Effects (Slam())
import SlamData.FileSystem.Dialog.Mount.Common.Render
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery
import SlamData.FileSystem.Dialog.Mount.MongoDB.Component.State
import SlamData.FileSystem.Resource (Mount(..))
import SlamData.Render.CSS as Rc

type Query = SettingsQuery State

comp :: Component State Query Slam
comp = component render eval

render :: State -> ComponentHTML Query
render state =
  H.div
    [ P.key "mount-mongodb"
    , P.class_ Rc.mountMongoDB
    ]
    [ section "Server(s)" [ hosts state ]
    , section "Authentication" [ userInfo state, fldPath state ]
    , section "Settings" [ propList _props state ]
    ]

eval :: Natural Query (ComponentDSL State Query Slam)
eval (ModifyState f next) = modify (processState <<< f) $> next
eval (Validate k) = do
  state <- get
  pure $ k
    if null (filter (not isEmptyHost) state.hosts)
    then Just "Please enter at least one host"
    else Nothing
eval (Submit parent name k) = do
  st <- get
  let path = parent </> dir name
  result <- liftAff $ attempt $ Auth.authed $ API.saveMount path (mkURI st)
  pure $ k $ map (const (Database path)) result

hosts :: State -> ComponentHTML Query
hosts state =
  H.div
    [ P.class_ Rc.mountHostList ]
    $ host state <$> 0 .. (length state.hosts - 1)

host :: State -> Int -> ComponentHTML Query
host state index =
  H.div
    [ P.class_ Rc.mountHost ]
    [ label "Host" [ input' rejectNonHostname state (_hosts <<< ix index <<< _host) [] ]
    , label "Port" [ input' rejectNonPort state (_hosts <<< ix index <<< _port) [] ]
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

userInfo :: State -> ComponentHTML Query
userInfo state =
  H.div
    [ P.classes [B.formGroup, Rc.mountUserInfo] ]
    [ fldUser state, fldPass state ]

fldUser :: State -> ComponentHTML Query
fldUser state = label "Username" [ input state _user [] ]

fldPass :: State -> ComponentHTML Query
fldPass state = label "Password" [ input state _password [ P.inputType P.InputPassword ] ]

fldPath :: State -> ComponentHTML Query
fldPath state =
  H.div
    [ P.class_ Rc.mountPath ]
    [ label "Database" [ input state _path [] ] ]

-- | A labelled section within the form.
label :: forall i p. String -> Array (HTML p i) -> HTML p i
label text inner = H.label_ $ [ H.span_ [ H.text text ] ] ++ inner

-- | A basic text input field that uses a lens to read from and update the
-- | state.
input
  :: forall p
   . State
  -> TraversalP State String
  -> Array (Cp.InputProp Query)
  -> HTML p Query
input state lens = input' id state lens -- can't eta reduce further here as the typechecker doesn't like it

-- | A basic text input field that uses a lens to read from and update the
-- | state, and allows for the input value to be modified.
input'
  :: forall p
   . (String -> String)
  -> State
  -> TraversalP State String
  -> Array (Cp.InputProp Query)
  -> HTML p Query
input' f state lens attrs =
  H.input
    $ [ P.class_ B.formControl
      , E.onValueInput (E.input \val -> ModifyState (lens .~ f val))
      , P.value (state ^. lens)
      ]
    ++ attrs
