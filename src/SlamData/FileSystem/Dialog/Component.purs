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
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Dialog.Error.Component as Error
import SlamData.FileSystem.Dialog.Download.Component as Download
import SlamData.FileSystem.Dialog.Explore.Component as Explore
import SlamData.FileSystem.Dialog.Mount.Component (MountSettings)
import SlamData.FileSystem.Dialog.Mount.Component as Mount
import SlamData.FileSystem.Dialog.Rename.Component as Rename
import SlamData.FileSystem.Dialog.Share.Component as Share
import SlamData.FileSystem.Resource (Resource)
import SlamData.Monad (Slam)
import SlamData.Workspace.Deck.Component.CSS as CSS

import Utils.Path (DirPath, FilePath)

data Dialog
  = Error String
  | Share String
  | Rename Resource
  | Mount DirPath String (Maybe MountSettings)
  | Download Resource
  | Explore FilePath

type State = Maybe Dialog

initialState ∷ State
initialState = Nothing

data Query a
  = Dismiss a
  | Show Dialog a


type ChildState =
  Mount.StateP
  ⊹ Download.State
  ⊹ Error.State
  ⊹ Share.State
  ⊹ Rename.State
  ⊹ Explore.State

type ChildQuery =
  Mount.QueryP
  ⨁ Download.Query
  ⨁ Error.Query
  ⨁ Share.Query
  ⨁ Rename.Query
  ⨁ Explore.Query

type ChildSlot =
  Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit

cpMount
  ∷ ChildPath
       Mount.StateP ChildState
       Mount.QueryP ChildQuery
       Unit ChildSlot
cpMount = cpL

cpDownload
  ∷ ChildPath
       Download.State ChildState
       Download.Query ChildQuery
       Unit ChildSlot
cpDownload = cpR :> cpL

cpError
  ∷ ChildPath
       Error.State ChildState
       Error.Query ChildQuery
       Unit ChildSlot
cpError = cpR :> cpR :> cpL

cpShare
  ∷ ChildPath
       Share.State ChildState
       Share.Query ChildQuery
       Unit ChildSlot
cpShare = cpR :> cpR :> cpR :> cpL

cpRename
  ∷ ChildPath
       Rename.State ChildState
       Rename.Query ChildQuery
       Unit ChildSlot
cpRename = cpR :> cpR :> cpR :> cpR :> cpL

cpExplore
  ∷ ChildPath
      Explore.State ChildState
      Explore.Query ChildQuery
      Unit ChildSlot
cpExplore = cpR :> cpR :> cpR :> cpR :> cpR


type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot
type QueryP = Query ⨁ (H.ChildF ChildSlot ChildQuery)
type DialogDSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot

comp ∷ H.Component StateP QueryP Slam
comp =
  H.parentComponent
    { render
    , eval
    , peek: Just (peek <<< H.runChildF)
    }

render ∷ State → H.ParentHTML ChildState Query ChildQuery Slam ChildSlot
render state =
  HH.div_
    [ HH.div
        [ HP.classes $ [ CSS.dialogBackdrop ] ⊕ (guard (isNothing state) $> CSS.invisible)
        , HE.onMouseDown (HE.input_ Dismiss)
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
  dialog (Error str) =
    HH.slot' cpError unit \_ →
      { component: Error.comp
      , initialState: Error.State str
      }
  dialog (Share str) =
    HH.slot' cpShare unit \_ →
      { component: Share.comp
      , initialState: Share.State str
      }
  dialog (Rename res) =
    HH.slot' cpRename unit \_ →
      { component: Rename.comp
      , initialState: Rename.initialState res
      }
  dialog (Download res) =
    HH.slot' cpDownload unit \_ →
      { component: Download.comp
      , initialState: Download.initialState res
      }
  dialog (Mount parent name settings) =
    HH.slot' cpMount unit \_ →
      { component: Mount.comp
      , initialState: H.parentState (Mount.initialState parent name settings)
      }
  dialog (Explore fp) =
    HH.slot' cpExplore unit \_ →
      { component: Explore.comp
      , initialState: Explore.initialState fp
      }

eval ∷ Query ~> H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot
eval (Dismiss next) = H.set Nothing $> next
eval (Show d next) = H.set (Just d) $> next

-- | Children can only close dialog. Other peeking in `FileSystem`
peek ∷ ∀ a. ChildQuery a → DialogDSL Unit
peek =
  mountPeek
  ⨁ downloadPeek
  ⨁ errorPeek
  ⨁ sharePeek
  ⨁ renamePeek
  ⨁ explorePeek

errorPeek ∷ ∀ a. Error.Query a → DialogDSL Unit
errorPeek (Error.Dismiss _) = H.set Nothing

sharePeek ∷ ∀ a. Share.Query a → DialogDSL Unit
sharePeek (Share.Dismiss _) = H.set Nothing
sharePeek _ = pure unit

renamePeek ∷ ∀ a. Rename.Query a → DialogDSL Unit
renamePeek (Rename.Dismiss _) = H.set Nothing
renamePeek _ = pure unit

mountPeek ∷ ∀ a. Mount.QueryP a → DialogDSL Unit
mountPeek = go ⨁ const (pure unit)
  where
  go (Mount.Dismiss _) = H.set Nothing
  go _ = pure unit

downloadPeek ∷ ∀ a. Download.Query a → DialogDSL Unit
downloadPeek (Download.Dismiss _) = H.set Nothing
downloadPeek _ = pure unit

explorePeek ∷ ∀ a. Explore.Query a → DialogDSL Unit
explorePeek (Explore.Dismiss _) = H.set Nothing
explorePeek _ = pure unit
