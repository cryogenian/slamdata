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

import SlamData.Dialog.Error.Component as Error
import SlamData.FileSystem.Dialog.Download.Component as Download
import SlamData.FileSystem.Dialog.Explore.Component as Explore
import SlamData.FileSystem.Dialog.Mount.Component (MountSettings)
import SlamData.FileSystem.Dialog.Mount.Component as Mount
import SlamData.FileSystem.Dialog.Rename.Component as Rename
import SlamData.FileSystem.Dialog.Share.Component as Share
import SlamData.FileSystem.Dialog.Component.Message (Message(..))
import SlamData.FileSystem.Resource (Resource)
import SlamData.Monad (Slam)
import SlamData.Workspace.Deck.Component.CSS as CSS

import Utils.Path (DirPath, FilePath)

data Dialog
  = Error String
  | Share String
  | Rename Resource
  | Mount DirPath String (Maybe MountSettings)
  | Download Resource (Array RequestHeader)
  | Explore FilePath

type State = Maybe Dialog

data Query a
  = Show Dialog a
  | RaiseDismiss a

type ChildQuery
  = Error.Query
  ⨁ Share.Query
  ⨁ Rename.Query
  ⨁ Download.Query
  ⨁ Mount.Query
  ⨁ Explore.Query
  ⨁ Const Void

type ChildSlot = Unit ⊹ Unit ⊹ Unit ⊹ Unit ⊹ Unit ⊹ Unit ⊹ Void

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
        [ HP.classes $ [ CSS.dialogBackdrop ] ⊕ (guard (isNothing state) $> CSS.invisible)
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
      HH.slot' CP.cp2 unit Share.component str (HE.input_ RaiseDismiss)
    Rename res →
      HH.slot' CP.cp3 unit Rename.component res (HE.input_ RaiseDismiss)
    Download resource headers →
      HH.slot' CP.cp4 unit Download.component { resource, headers } (HE.input_ RaiseDismiss)
    Mount parent name settings →
      HH.slot' CP.cp5 unit Mount.component { parent, name, settings } (HE.input_ RaiseDismiss)
    Explore fp →
      HH.slot' CP.cp6 unit Explore.component fp (HE.input_ RaiseDismiss)

eval ∷ Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message Slam
eval = case _ of
  Show d next → H.put (Just d) $> next
  RaiseDismiss next → do
    H.put Nothing
    H.raise Dismiss
    pure next
