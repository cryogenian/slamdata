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

module SlamData.Dialog.Share.Permissions.Component where

import SlamData.Prelude

import Data.Lens (LensP, lens, (%~), (.~))

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.Quasar.Auth.Permission (Permissions)
import SlamData.Render.CSS as Rc

notAllowed :: Permissions
notAllowed =
  { add: false
  , read: false
  , modify: false
  , delete: false
  }

type State =
  { current :: Permissions
  , max :: Permissions
  }

initialState :: State
initialState =
  { current: notAllowed
  , max: notAllowed
  }

_current :: forall a r. LensP {current :: a|r} a
_current = lens _.current _{current = _}

_max :: forall a r. LensP {max :: a|r} a
_max = lens _.max _{max = _}

_add :: forall a r. LensP {add :: a|r} a
_add = lens _.add _{add = _}

_read :: forall a r. LensP {read :: a|r} a
_read = lens _.read _{read = _}

_modify :: forall a r. LensP {modify :: a|r} a
_modify = lens _.modify _{modify = _}

_delete :: forall a r. LensP {delete :: a|r} a
_delete = lens _.delete _{delete = _}

data Query a
  = ToggleAdd a
  | ToggleRead a
  | ToggleModify a
  | ToggleDelete a
  | SetMaximum Permissions a
  | GetSelected (Permissions -> a)

type PermissionsDSL = H.ComponentDSL State Query Slam

comp :: H.Component State Query Slam
comp = H.component { render, eval }

render :: State -> H.ComponentHTML Query
render { current, max } =
  HH.form
    [ HP.classes [ Rc.sharePermissionsCheckboxes ] ]
    [ HH.label
        [ HP.classes [ B.checkboxInline ] ]
        [ HH.input
            [ HP.inputType HP.InputCheckbox
            , HP.checked (max.add && current.add)
            , HP.disabled $ not max.add
            , HE.onChecked (HE.input_ ToggleAdd)
            ]
        , HH.text "Add"
        ]
    , HH.label
        [ HP.classes [ B.checkboxInline ] ]
        [ HH.input
            [ HP.inputType HP.InputCheckbox
            , HP.checked (max.read && current.read)
            , HP.disabled $ not max.read
            , HE.onChecked (HE.input_ ToggleRead)
            ]
        , HH.text "Read"
        ]
    , HH.label
        [ HP.classes [ B.checkboxInline ] ]
        [ HH.input
            [ HP.inputType HP.InputCheckbox
            , HP.checked (max.modify && current.modify)
            , HP.disabled $ not max.modify
            , HE.onChecked (HE.input_ ToggleModify)
            ]
        , HH.text "Modify"
        ]
    , HH.label
        [ HP.classes [ B.checkboxInline ] ]
        [ HH.input
            [ HP.inputType HP.InputCheckbox
            , HP.checked (max.delete && current.delete)
            , HP.disabled $ not max.delete
            , HE.onChecked (HE.input_ ToggleDelete)
            ]
        , HH.text "Delete"
        ]
    ]

eval :: Natural Query PermissionsDSL
eval (ToggleAdd next) = H.modify (_current <<< _add %~ not) $> next
eval (ToggleRead next) = H.modify (_current <<< _read %~ not) $> next
eval (ToggleModify next) = H.modify (_current <<< _modify %~ not) $> next
eval (ToggleDelete next) = H.modify (_current <<< _delete %~ not) $> next
eval (SetMaximum m next) = H.modify (_max .~ m) $> next
eval (GetSelected continue) = map continue $ H.gets _.current
