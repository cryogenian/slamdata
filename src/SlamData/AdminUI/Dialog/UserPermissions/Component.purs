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

module SlamData.AdminUI.Dialog.UserPermissions.Component (dialog) where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
import Quasar.Advanced.Types as QA
import SlamData.Dialog.Component as D
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN

data Query a
  = Delete QA.GroupPath a
  | Add QA.GroupPath a
  | Dismiss a

type State =
  { groups ∷ Array QA.GroupPath
  , allGroups ∷ Array QA.GroupPath
  , refreshing ∷ Boolean
  }

initialState ∷ State
initialState = { groups: [], allGroups: [], refreshing: true }

type Message o = Variant o
type HTML = H.ComponentHTML Query
type DSL o =  H.ComponentDSL State Query (D.Message (Message o)) Slam

dialog ∷ ∀ o. QA.UserId → D.DialogSpec (Message o) Slam
dialog userId =
  D.dialog
    $ D.withTitle "Edit Permissions"
    >>> D.withInitialState initialState
    >>> D.withClass (H.ClassName "sd-admin-ui-user-permissions-dialog")
    >>> D.withRender render
    >>> D.withEval eval
    >>> D.withPending (_.refreshing)
    >>> D.withButton
        (D.button
          $ D.withLabel "Confirm"
          >>> D.withClass CN.btnPrimary
          >>> D.withAction (const (Just Dismiss)))
  where
    render ∷ State → HTML
    render _ =
      HH.text "hi"
    eval ∷ Query ~> DSL o
    eval = case _ of
      Delete group next → pure next
      Add group next → pure next
      Dismiss next → pure next
