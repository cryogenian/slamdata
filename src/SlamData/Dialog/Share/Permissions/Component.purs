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

import Prelude

import Data.Functor (($>))
import Data.Lens (LensP(), lens, (%~), (.~))

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam())

type Permissions =
  {
    add :: Boolean
  , read :: Boolean
  , modify :: Boolean
  , delete :: Boolean
  }

notAllowed :: Permissions
notAllowed =
  {
    add: false
  , read: false
  , modify: false
  , delete: false
  }

type State =
  {
    current :: Permissions
  , max :: Permissions
  }

initialState :: State
initialState =
  {
    current: notAllowed
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

type PermissionsDSL = ComponentDSL State Query Slam

comp :: Component State Query Slam
comp = component render eval

render :: State -> ComponentHTML Query
render {current, max} =
  H.form_
    [
      H.label [ P.classes [ B.checkboxInline ] ]
        [
          H.input [ P.inputType P.InputCheckbox
                  , P.checked (max.add && current.add)
                  , P.disabled $ not max.add
                  , E.onChecked (E.input_ ToggleAdd)
                  ]
        , H.text  "Add"
        ]
    , H.label [ P.classes [ B.checkboxInline ] ]
        [
          H.input [ P.inputType P.InputCheckbox
                  , P.checked (max.read && current.read)
                  , P.disabled $ not max.read
                  , E.onChecked (E.input_ ToggleRead)
                  ]
        , H.text "Read"
        ]
    , H.label [ P.classes [ B.checkboxInline ] ]
        [
          H.input [ P.inputType P.InputCheckbox
                  , P.checked (max.modify && current.modify)
                  , P.disabled $ not max.modify
                  , E.onChecked (E.input_ ToggleModify)
                  ]
        , H.text "Modify"
        ]
    , H.label [ P.classes [ B.checkboxInline ] ]
        [
          H.input [ P.inputType P.InputCheckbox
                  , P.checked (max.delete && current.delete)
                  , P.disabled $ not max.delete
                  , E.onChecked (E.input_ ToggleDelete)
                  ]
        , H.text "Delete"
        ]
    ]

eval :: Natural Query PermissionsDSL
eval (ToggleAdd next) = modify (_current <<< _add %~ not) $> next
eval (ToggleRead next) = modify (_current <<< _read %~ not) $> next
eval (ToggleModify next) = modify (_current <<< _modify %~ not) $> next
eval (ToggleDelete next) = modify (_current <<< _delete %~ not) $> next
eval (SetMaximum m next) = modify (_max .~ m) $> next
eval (GetSelected continue) = map continue $ gets _.current
