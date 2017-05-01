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

module SlamData.Workspace.Card.Error.Component
  ( errorCardComponent
  , module SlamData.Workspace.Card.Error.Component.Query
  , module SlamData.Workspace.Card.Error.Component.State
  ) where

import SlamData.Prelude

import Data.Path.Pathy as Path
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Quasar.Advanced.QuasarAF as QA
import SlamData.GlobalError as GE
import SlamData.Monad (Slam)
import SlamData.Render.CSS as CSS
import SlamData.Workspace.Card.Cache.Error as CCE
import SlamData.Workspace.Card.Error (CardError(..), cardToGlobalError)
import SlamData.Workspace.Card.Error.Component.Query (Query, initiality)
import SlamData.Workspace.Card.Error.Component.State (State, initialState)

type DSL = H.ComponentDSL State Query Void Slam
type HTML = H.ComponentHTML Query

errorCardComponent ∷ H.Component HH.HTML Query CardError Void Slam
errorCardComponent =
  H.component
    { initialState: initialState
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ State → HTML
render st =
  HH.div
    [ HP.classes [ CSS.cardFailures ] ]
    [ HH.text (prettyPrintCardError st.error) ]

eval ∷ Query ~> DSL
eval = initiality

prettyPrintCardError ∷ CardError → String
prettyPrintCardError ce = case cardToGlobalError ce of
  Just ge → GE.print ge
  Nothing → case ce of
    QuasarError qError → QA.printQError qError
    StringlyTypedError err → err
    CacheCardError cce → cacheErrorMessage cce

cacheErrorMessage ∷ CCE.CacheError → String
cacheErrorMessage = case _ of
   CCE.CacheInvalidFilepath fp →
     fp <> " is not a valid file path"
   CCE.CacheQuasarError qe →
     "Encountered a Quasar Error while verifying the cache result: "
     <> QA.printQError qe
   CCE.CacheErrorSavingFile →
     "Error saving file, please try another location"
   CCE.CacheResourceNotModified outputResource →
     -- TODO: this error message is pretty obscure. I think it occurs when a query
     -- is like "SELECT * FROM t" and quasar does no work. I'm not sure what the
     -- behaviour of Save should be in that case - perhaps instead of failing it
     -- could create a view so that a resource will actually be created. Debateable
     -- as to whether that is "right", but at least it means a resource will exist
     -- in the expected location, and the rest of the deck can run as the Save
     -- failing has not effect on the output. -gb
     "Resource: " <> Path.printPath outputResource <> " hasn't been modified"
