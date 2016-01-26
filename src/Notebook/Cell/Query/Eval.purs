{-
Copyright 2015 SlamData, Inc.

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

module Notebook.Cell.Query.Eval
  ( queryEval
  , querySetup
  ) where

import Prelude

import Control.Monad.Error.Class as EC
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans as MT
import Control.Monad.Writer.Class as WC

import Data.Either as E
import Data.Foldable as F
import Data.Functor.Aff (liftAff)
import Data.Functor.Eff (liftEff)
import Data.Lens as L
import Data.Maybe (Maybe(..))
import Data.Maybe as M
import Data.Path.Pathy as Path
import Data.StrMap as SM
import Data.String as Str

import Halogen (query, action, request)

import Model.Resource as R
import Notebook.Cell.Ace.Component (AceDSL())
import Notebook.Cell.Common.EvalQuery as CEQ
import Notebook.Cell.Port as Port
import Quasar.Aff as Quasar
import Utils.Completions (mkCompletion, pathCompletions)

import Ace.Halogen.Component as Ace
import Ace.Types (Completion())
import Utils.Ace (readOnly)

queryEval :: CEQ.CellEvalInput -> String -> AceDSL CEQ.CellEvalResult
queryEval info sql = do
  addCompletions varMap
  liftAff $ CEQ.runCellEvalT $ do
    { plan: plan, outputResource: outputResource } <-
      Quasar.executeQuery sql
        (M.fromMaybe false info.cachingEnabled) varMap inputResource tempOutputResource
        # MT.lift
        >>= E.either EC.throwError pure

    F.for_ plan \p ->
      WC.tell ["Plan: " <> p]

    pure $ Port.Resource outputResource
  where
  varMap :: SM.StrMap String
  varMap =
    info.inputPort
    >>= L.preview Port._VarMap
    # M.maybe SM.empty (map Port.renderVarMapValue)

  tempOutputResource = CEQ.temporaryOutputResource info
  inputResource = R.parent tempOutputResource -- TODO: make sure that this is actually still correct


querySetup :: CEQ.CellSetupInfo -> AceDSL Unit
querySetup { inputPort, notebookPath } =
  case inputPort of
    Port.VarMap varMap -> addCompletions varMap
    Port.Resource res -> void $ runMaybeT do
      resParent <- MaybeT $ pure $ L.preview R._filePath res >>= Path.parentDir
      nbPath <- MaybeT $ pure notebookPath
      let path = if nbPath == resParent
                 then R.resourceName res
                 else R.resourcePath res
      editor <- (MaybeT $ query unit $ request Ace.GetEditor) >>= (MaybeT <<< pure)
      MaybeT $ query unit
        $ action $ Ace.SetText ("SELECT  *  FROM \"" <> path <> "\" ")
      MT.lift $ liftEff do
        readOnly editor { startRow: 0
                        , startColumn: 0
                        , endRow: 0
                        , endColumn: 7
                        }
        readOnly editor { startRow: 0
                        , startColumn: 10
                        , endRow: 0
                        , endColumn: 19 + Str.length path
                        }
    _ -> pure unit

addCompletions :: forall a. SM.StrMap a -> AceDSL Unit
addCompletions vm =
  void $ query unit $ action $ Ace.SetCompleteFn \_ _ _ inp -> do
    let compl = varMapCompletions vm
    paths <- pathCompletions
    pure $ compl <> paths

  where
  varMapCompletions :: SM.StrMap a -> Array Completion
  varMapCompletions strMap =
    SM.keys strMap <#> mkCompletion "variable" (Just <<< append ":")
