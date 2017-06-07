{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Dialog.Component
  ( Query(..)
  , Message(..)
  , ChildSlot
  , component
  , module SlamData.Workspace.Dialog.Types
  ) where

import SlamData.Prelude
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Dialog.Error.Component as Error
import SlamData.Dialog.Render (licenseExpired, licenseInvalid)
import SlamData.Monad (Slam)
import SlamData.Workspace.Deck.Options (DeckOptions)
import SlamData.Workspace.Dialog.Confirm.Component as Confirm
import SlamData.Workspace.Dialog.Export.Component as Export
import SlamData.Workspace.Dialog.Reason.Component as Reason
import SlamData.Workspace.Dialog.Rename.Component as Rename
import SlamData.Workspace.Dialog.Share.Component as Share
import SlamData.Workspace.Dialog.Types (Dialog(..))
import SlamData.Workspace.Dialog.Unshare.Component as Unshare
import SlamData.License as License

type State = Maybe Dialog

data Query a
  = Show Dialog a
  | Raise Message a

data Message
  = Dismissed
  | Confirm DeckOptions Dialog Boolean

type ChildQuery =
  Rename.Query
  ⨁ Error.Query
  ⨁ Confirm.Query
  ⨁ Export.Query
  ⨁ Export.Query
  ⨁ Share.Query
  ⨁ Unshare.Query
  ⨁ Reason.Query
  ⨁ Const Void

type ChildSlot =
  Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Void

type HTML = H.ParentHTML Query ChildQuery ChildSlot Slam

type DSL = H.ParentDSL State Query ChildQuery ChildSlot Message Slam

component ∷ H.Component HH.HTML Query Unit Message Slam
component =
  H.parentComponent
    { render
    , eval
    , initialState: const Nothing
    , receiver: const Nothing
    }

renderDialogBackdrop ∷ HTML
renderDialogBackdrop =
  HH.div
    [ HP.classes $ [ HH.ClassName "deck-dialog-backdrop" ]
    , HE.onClick $ HE.input_ $ Raise Dismissed
    ]
    [ ]

render ∷ State → HTML
render = case _ of
  Nothing ->
    HH.text ""
  Just s ->
    HH.div_
      [ renderDialogBackdrop
      , HH.div [ HP.classes [ HH.ClassName "deck-dialog" ] ] [ dialog s ]
      ]
  where
  dialog dlg = case dlg of
    Rename opts name →
      HH.slot' CP.cp1 unit Rename.component name
        case _ of
          Rename.Dismiss → Just $ H.action $ Raise Dismissed
          Rename.Rename name' → Just $ H.action $ Raise (Confirm opts (Rename opts name') true)

    Error _ str →
      HH.slot' CP.cp2 unit Error.nonModalComponent str
        \Error.Dismiss → Just $ H.action $ Raise Dismissed

    DeleteDeck opts →
      HH.slot' CP.cp3 unit Confirm.component
        { title: "Delete deck"
        , body: "Are you sure you want to delete this deck?"
        , cancel: "Cancel"
        , confirm: "Delete"
        }
        \(Confirm.Confirm bool) → Just $ H.action $ Raise (Confirm opts dlg bool)

    Embed _ sharingInput varMaps →
      HH.slot' CP.cp4 unit Export.component
        { sharingInput
        , presentingAs: Export.Embed
        , varMaps
        }
        \Export.Dismiss → Just $ H.action $ Raise Dismissed

    Publish _ sharingInput varMaps →
      HH.slot' CP.cp5 unit Export.component
        { sharingInput
        , presentingAs: Export.Publish
        , varMaps
        }
        \Export.Dismiss → Just $ H.action $ Raise Dismissed

    Share _ sharingInput →
      HH.slot' CP.cp6 unit Share.component sharingInput
        \Share.Dismiss → Just $ H.action $ Raise Dismissed

    Unshare _ sharingInput →
      HH.slot' CP.cp7 unit Unshare.component sharingInput
        \Unshare.Dismiss → Just $ H.action $ Raise Dismissed

    Reason _ attemptedCardType reason cardPaths →
      HH.slot' CP.cp8 unit Reason.component
        { attemptedCardType
        , reason
        , cardPaths
        }
        \Reason.Dismiss → Just $ H.action $ Raise Dismissed

    LicenseProblem License.Expired →
      licenseExpired

    LicenseProblem License.Invalid →
      licenseInvalid

eval ∷ Query ~> DSL
eval = case _ of
  Show dialog next → H.put (Just dialog) $> next
  Raise msg next → H.put Nothing *> H.raise msg $> next
