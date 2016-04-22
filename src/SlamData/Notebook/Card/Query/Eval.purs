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

module SlamData.Notebook.Card.Query.Eval
  ( queryEval
  , querySetup
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Exception as Exn
import Control.Monad.Error.Class as EC
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Writer.Class as WC

import Data.Lens as L
import Data.Path.Pathy as Path
import Data.String as Str
import Data.StrMap as SM

import Ace.Halogen.Component as Ace
import Ace.Types (Completion)

import Halogen (query, action, request, fromEff)

import SlamData.Notebook.Card.Ace.Component (AceDSL)
import SlamData.Notebook.Card.Common.EvalQuery as CEQ
import SlamData.Notebook.Card.Port as Port
import SlamData.Quasar.FS (messageIfFileNotFound) as Quasar
import SlamData.Quasar.Query (viewQuery, compile) as Quasar

import Utils.Ace (readOnly)
import Utils.Completions (mkCompletion, pathCompletions)

queryEval ∷ CEQ.CardEvalInput → String → AceDSL CEQ.CardEvalResult
queryEval info sql =
  case info.inputPort of
    Just Port.Blocked →
      pure { output: Nothing, messages: [] }
    _ → do
      addCompletions varMap
      CEQ.runCardEvalT do

        plan ← lift $ CEQ.liftWithCancelerP $
          Quasar.compile backendPath sql varMap

        Quasar.viewQuery
            backendPath
            outputResource
            sql
            varMap
          # CEQ.liftWithCancelerP
          # lift
          >>= either (EC.throwError <<< Exn.message) pure

        Quasar.messageIfFileNotFound
            outputResource
            "Requested collection doesn't exist"
          # CEQ.liftWithCancelerP
          # lift
          >>= either (EC.throwError <<< Exn.message) (traverse EC.throwError)

        for_ plan \p → WC.tell ["Plan: " ⊕ p]

        pure $ Port.TaggedResource { resource: outputResource, tag: pure sql }
  where
  varMap ∷ SM.StrMap String
  varMap =
    info.inputPort
    >>= L.preview Port._VarMap
    # maybe SM.empty (map Port.renderVarMapValue)

  outputResource = CEQ.temporaryOutputResource info
  backendPath = Left $ fromMaybe Path.rootDir (Path.parentDir outputResource)

querySetup ∷ CEQ.CardSetupInfo → AceDSL Unit
querySetup { inputPort, notebookPath } =
  case inputPort of
    Port.VarMap varMap →
      addCompletions varMap

    Port.TaggedResource {resource} → void $ runMaybeT do
      resParent ← MaybeT $ pure $ Path.parentDir resource

      let
        path = if notebookPath ≡ pure resParent
                 then Path.runFileName (Path.fileName resource)
                 else Path.printPath resource
      editor ←
        (MaybeT $ query unit $ request Ace.GetEditor)
        >>= (MaybeT ∘ pure)

      MaybeT
        $ query unit
        $ action
        $ Ace.SetText ("SELECT  *  FROM `" ⊕ path ⊕ "` ")

      lift $ fromEff do
        readOnly editor
          { startRow: 0
          , startColumn: 0
          , endRow: 0
          , endColumn: 7
          }
        readOnly editor
          { startRow: 0
          , startColumn: 10
          , endRow: 0
          , endColumn: 19 + Str.length path
          }
    _ → pure unit

addCompletions ∷ ∀ a. SM.StrMap a → AceDSL Unit
addCompletions vm =
  void $ query unit $ action $ Ace.SetCompleteFn \_ _ _ inp → do
    let compl = varMapCompletions vm
    paths ← pathCompletions
    pure $ compl ⊕ paths

  where
  varMapCompletions ∷ SM.StrMap a → Array Completion
  varMapCompletions strMap =
    SM.keys strMap <#> mkCompletion "variable" (Just ∘ append ":")
