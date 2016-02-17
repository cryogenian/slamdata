module SlamData.Dialog.Share.Permissions where

import Prelude

import Data.Functor (($>))
import Data.Lens (LensP(), lens, (%~))

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam())

type State =
  {
    add :: Boolean
  , read :: Boolean
  , modify :: Boolean
  , delete :: Boolean
  }

initialState :: State
initialState =
  {
    add: false
  , read: false
  , modify: false
  , delete: false
  }

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
  | GetState (State -> a)

type PermissionsDSL = ComponentDSL State Query Slam

comp :: forall e. Component State Query Slam
comp = component render eval

render :: State -> ComponentHTML Query
render state =
  H.form_
    [
      H.label [ P.classes [ B.checkboxInline ] ]
        [
          H.input [ P.inputType P.InputCheckbox
                  , P.checked state.add
                  , E.onChecked (E.input_ ToggleAdd)
                  ]
        , H.text  "Add"
        ]
    , H.label [ P.classes [ B.checkboxInline ] ]
        [
          H.input [ P.inputType P.InputCheckbox
                  , P.checked state.read
                  , E.onChecked (E.input_ ToggleRead)
                  ]
        , H.text "Read"
        ]
    , H.label [ P.classes [ B.checkboxInline ] ]
        [
          H.input [ P.inputType P.InputCheckbox
                  , P.checked state.modify
                  , E.onChecked (E.input_ ToggleModify)
                  ]
        , H.text "Modify"
        ]
    , H.label [ P.classes [ B.checkboxInline ] ]
        [
          H.input [ P.inputType P.InputCheckbox
                  , P.checked state.delete
                  , E.onChecked (E.input_ ToggleDelete)
                  ]
        , H.text "Delete"
        ]
    ]

eval :: Natural Query PermissionsDSL
eval (ToggleAdd next) = modify (_add %~ not) $> next
eval (ToggleRead next) = modify (_read %~ not) $> next
eval (ToggleModify next) = modify (_modify %~ not) $> next
eval (ToggleDelete next) = modify (_delete %~ not) $> next
eval (GetState continue) = map continue get
