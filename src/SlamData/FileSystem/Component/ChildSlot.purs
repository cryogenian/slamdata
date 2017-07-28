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

module SlamData.FileSystem.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath as CP
import SlamData.AdminUI.Types as AdminUI
import SlamData.Dialog.License.Component as LicenseDialog
import SlamData.FileSystem.Dialog as Dialog
import SlamData.FileSystem.Listing.Component as Listing
import SlamData.FileSystem.Search.Component as Search
import SlamData.Header.Component as Header
import SlamData.Notification.Component as Notify

type ChildQuery
  = Listing.Query
  ⨁ Search.Query
  ⨁ LicenseDialog.Query
  ⨁ Header.Query
  ⨁ Notify.Query
  ⨁ AdminUI.Query
  ⨁ Dialog.Query
  ⨁ Const Void

type ChildSlot
  = Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Void

cpListing
  ∷ CP.ChildPath
      Listing.Query ChildQuery
      Unit ChildSlot
cpListing = CP.cp1

cpSearch
  ∷ CP.ChildPath
      Search.Query ChildQuery
      Unit ChildSlot
cpSearch = CP.cp2

cpLicenseDialog
  ∷ CP.ChildPath
      LicenseDialog.Query ChildQuery
      Unit ChildSlot
cpLicenseDialog = CP.cp3

cpHeader
  ∷ CP.ChildPath
      Header.Query ChildQuery
      Unit ChildSlot
cpHeader = CP.cp4

cpNotify
  ∷ CP.ChildPath
      Notify.Query ChildQuery
      Unit ChildSlot
cpNotify = CP.cp5

cpAdminUI
  ∷ CP.ChildPath
      AdminUI.Query ChildQuery
      Unit ChildSlot
cpAdminUI = CP.cp6

cpDialog
  ∷ CP.ChildPath
      Dialog.Query ChildQuery
      Unit ChildSlot
cpDialog = CP.cp7
