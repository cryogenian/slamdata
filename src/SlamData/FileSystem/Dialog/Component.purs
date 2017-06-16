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

module SlamData.FileSystem.Dialog.Component where

import SlamData.Prelude

import Data.Array (singleton)

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import Network.HTTP.RequestHeader (RequestHeader)

import Quasar.Advanced.Types as QAT

import SlamData.Dialog.Error.Component as Error
import SlamData.Dialog.License (advancedLicenseExpired, advancedTrialLicenseExpired, licenseInvalid)
import SlamData.FileSystem.Dialog.Component.Message (Message(..))
import SlamData.FileSystem.Dialog.Download.Component as Download
import SlamData.FileSystem.Dialog.Mount.Component as Mount
import SlamData.FileSystem.Dialog.Rename.Component as Rename
import SlamData.FileSystem.Dialog.Share.Component as Share
import SlamData.FileSystem.Resource (Resource, Mount)
import SlamData.License as License
import SlamData.Monad (Slam)
import SlamData.Workspace.Deck.Component.CSS as CSS

data Dialog
  = Error String
  | Share String
  | Rename Resource
  | Mount Mount.Input
  | Download Resource (Array RequestHeader)
  | LicenseProblem License.LicenseProblem

type State = Maybe Dialog

data Query a
  = Show Dialog a
  | RaiseDismiss a
  | QueryRename (Rename.Query Unit) a
  | SaveMount (Maybe Mount → a)
  | HandleChild Message a
  | AddDirsToRename (Array Resource) a

type ChildQuery
  = Error.Query
  ⨁ Share.Query
  ⨁ Rename.Query
  ⨁ Download.Query
  ⨁ Mount.Query
  ⨁ Const Void

type ChildSlot = Unit ⊹ Unit ⊹ Unit ⊹ Unit ⊹ Unit ⊹ Void

component ∷ H.Component HH.HTML Query Unit Message Slam
component =
  H.parentComponent
    { initialState: const Nothing
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ State → H.ParentHTML Query ChildQuery ChildSlot Slam
render state =
  HH.div_
    [ HH.div
        [ HP.classes $ [ HH.ClassName "deck-dialog-backdrop" ] ⊕ (guard (isNothing state) $> CSS.invisible)
        , HE.onMouseDown (HE.input_ RaiseDismiss)
        , ARIA.hidden $ show $ isNothing state
        ]
        []
    , HH.div
        [ HP.classes $ [] ⊕ (guard (isNothing state) $> CSS.invisible)
        , ARIA.hidden $ show $ isNothing state
        ]
        $ maybe [] (singleton <<< dialog) state
    ]
  where
  dialog = case _ of
    Error str →
      HH.slot' CP.cp1 unit Error.component str (HE.input_ RaiseDismiss)
    Share str →
      HH.slot' CP.cp2 unit Share.component str (HE.input HandleChild)
    Rename res →
      HH.slot' CP.cp3 unit Rename.component res (HE.input HandleChild)
    Download resource headers →
      HH.slot' CP.cp4 unit Download.component { resource, headers } (HE.input HandleChild)
    Mount input →
      HH.slot' CP.cp5 unit Mount.component input (HE.input HandleChild)
    LicenseProblem (License.Expired licenseType) →
      case licenseType of
        QAT.Advanced → advancedLicenseExpired
        QAT.AdvancedTrial → advancedTrialLicenseExpired
    LicenseProblem License.Invalid →
      licenseInvalid

eval ∷ Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message Slam
eval = case _ of
  QueryRename q next → do
    _ ← H.query' CP.cp3 unit q
    pure next
  SaveMount reply → do
    map (reply ∘ join) $ H.query' CP.cp5 unit $ H.request Mount.Save
  Show d next → do
    H.put (Just d)
    pure next
  RaiseDismiss next → do
    H.put Nothing
    pure next
  HandleChild m next → do
    case m of
      Dismiss → H.put Nothing
      _ → H.raise m
    pure next
  AddDirsToRename dirs next → do
    _ ← H.query' CP.cp3 unit $ H.action $ Rename.AddDirs dirs
    pure next
