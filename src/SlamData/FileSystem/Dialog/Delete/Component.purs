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

module SlamData.FileSystem.Dialog.Delete.Component (dialog) where

import SlamData.Prelude

import Data.Variant as V
import Halogen as H
import Halogen.HTML as HH
import SlamData.Dialog.Component as D
import SlamData.Dialog.Render as DR
import SlamData.FileSystem.Resource as R
import SlamData.GlobalError as GE
import SlamData.Monad (Slam)
import SlamData.Quasar.FS as QFS
import SlamData.Render.ClassName as CN
import Utils.Path as UP

data Query a
  = Delete a
  | Dismiss a

type State =
  { resource ∷ R.Resource
  , deleting ∷ Boolean
  , error ∷ Maybe String
  }

initialState ∷ R.Resource → State
initialState = { resource: _, deleting: false, error: Nothing }

type Message o = Variant (deleted ∷ UP.AnyPath | o)
type HTML = H.ComponentHTML Query
type DSL o =  H.ComponentDSL State Query (D.Message (Message o)) Slam

dialog ∷ ∀ o. R.Resource → D.DialogSpec (Message o) Slam
dialog res =
  D.dialog
    $ D.withTitle "Confirm deletion"
    >>> D.withInitialState (initialState res)
    >>> D.withClass (H.ClassName "sd-delete-dialog")
    >>> D.withRender render
    >>> D.withEval eval
    >>> D.withPending (_.deleting)
    >>> D.withButton
        (D.button
          $ D.withLabel "Cancel"
          >>> D.withAction (const (Just Dismiss)))
    >>> D.withButton
        (D.button
          $ D.withLabel "Delete"
          >>> D.withClass CN.btnPrimary
          >>> D.withAction (const (Just Delete))
          >>> D.withPending (_.deleting))


render ∷ State → HTML
render { resource, error } =
  HH.div_
    $ join
        [ pure $ HH.div_
            [ HH.text "Are you sure you want delete "
            , HH.code_ [ HH.text (R.resourceName resource) ]
            , HH.text "?"
            ]
        , foldMap (pure ∘ DR.renderError) error
        ]

eval ∷ ∀ o. Query ~> DSL o
eval = case _ of
  Delete next → do
    { resource } ← H.get
    H.modify (_ { deleting = true })
    QFS.delete resource >>= case _ of
      Left err → do
        case GE.fromQError err of
          Left msg →
            H.modify (_
              { error = Just $ "There was a problem deleting the resource: " <> msg
              , deleting = false
              })
          Right ge → do
            H.modify (_ { deleting = false })
            GE.raiseGlobalError ge
      Right _ → do
        H.raise (D.Bubble (V.inj _deleted (R.getPath resource)))
        H.raise D.Dismiss
    pure next
  Dismiss next → do
    H.raise D.Dismiss
    pure next

_deleted ∷ SProxy "deleted"
_deleted = SProxy
