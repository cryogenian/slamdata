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

module SlamData.Workspace.Card.Cache.Error where

import SlamData.Prelude
import Quasar.Types (FilePath)
import Data.Path.Pathy as Path
import Quasar.Advanced.QuasarAF (QError, printQError)

data CacheError
  = CacheInvalidFilepath String
  | CacheQuasarError QError
  | CacheErrorSavingFile
  | CacheResourceNotModified FilePath

cacheErrorMessage ∷ Warn "More structure please" ⇒ CacheError → String
cacheErrorMessage = case _ of
   CacheInvalidFilepath fp →
     fp <> " is not a valid file path"
   CacheQuasarError qe →
     "Encountered a Quasar Error while verifying the cache result: "
     <> printQError qe
   CacheErrorSavingFile →
     "Error saving file, please try another location"
   CacheResourceNotModified outputResource →
     -- TODO: this error message is pretty obscure. I think it occurs when a query
     -- is like "SELECT * FROM t" and quasar does no work. I'm not sure what the
     -- behaviour of Save should be in that case - perhaps instead of failing it
     -- could create a view so that a resource will actually be created. Debateable
     -- as to whether that is "right", but at least it means a resource will exist
     -- in the expected location, and the rest of the deck can run as the Save
     -- failing has not effect on the output. -gb
     "Resource: " <> Path.printPath outputResource <> " hasn't been modified"
