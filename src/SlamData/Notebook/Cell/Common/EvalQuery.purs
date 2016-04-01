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

module SlamData.Notebook.Cell.Common.EvalQuery
  ( CellEvalQuery(..)
  , CellEvalResult
  , CellEvalResultP
  , CellEvalInputP
  , CellEvalInputPre
  , CellEvalInput
  , CellSetupInfoP
  , CellSetupInfo
  , CellEvalT
  , runCellEvalT
  , temporaryOutputResource
  , prepareCellEvalInput
  , liftWithCanceler
  , liftWithCanceler'
  , liftWithCancelerP
  , liftWithCancelerP'
  ) where

import SlamData.Prelude

import Control.Monad.Aff (Canceler)
import Control.Monad.Error.Class as EC
import Control.Monad.Except.Trans as ET
import Control.Monad.Writer.Class as WC
import Control.Monad.Writer.Trans as WT

import Data.Argonaut.Core (Json)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P

import SlamData.FileSystem.Resource as R
import SlamData.Notebook.Cell.CellId as CID
import SlamData.Notebook.Cell.Port (Port)
import SlamData.Notebook.Cell.Port.VarMap as Port
import SlamData.Effects (Slam, SlamDataEffects)

import Utils.Path (DirPath, FilePath)

import Halogen (ParentDSL, ComponentDSL)
import Halogen.Component.Utils as Hu

type CellEvalInputP r =
  { notebookPath ∷ Maybe DirPath
  , inputPort ∷ Maybe Port
  , cellId ∷ CID.CellId
  , globalVarMap ∷ Port.VarMap
  | r
  }

type CellEvalInputPre = CellEvalInputP ()
type CellEvalInput =
  CellEvalInputP
    ( cachingEnabled ∷ Maybe Boolean
    )

type CellSetupInfoP r =
  { notebookPath ∷ Maybe DirPath
  , inputPort ∷ Port
  , cellId ∷ CID.CellId
  | r}

type CellSetupInfo =
  CellSetupInfoP ()

prepareCellEvalInput
  ∷ Maybe Boolean
  → CellEvalInputPre
  → CellEvalInput
prepareCellEvalInput cachingEnabled { notebookPath, inputPort, cellId, globalVarMap } =
  { notebookPath
  , inputPort
  , cellId
  , cachingEnabled
  , globalVarMap
  }

temporaryOutputResource
  ∷ ∀ r
  . {notebookPath ∷ Maybe DirPath, cellId ∷ CID.CellId | r}
  → FilePath
temporaryOutputResource info =
  outputDirectory </> outputFile
  where
    outputDirectory =
      filterMaybe (_ == P.rootDir) info.notebookPath #
        fromMaybe (P.rootDir </> P.dir ".tmp")

    outputFile =
      P.file $ "out" ⊕ CID.cellIdToString info.cellId

    filterMaybe ∷ ∀ a. (a → Boolean) → Maybe a → Maybe a
    filterMaybe p m =
      m >>= \x →
        if p x then Nothing else pure x

-- | The query algebra shared by the inner parts of a cell component.
-- |
-- | - `EvalCell` is a command sent from the notebook that runs the cell. An
-- |   optional input value (the output from another cell) is provided, and a
-- |   continuation for the evaluation result to be returned to.
-- | - `SetupCell` is will be called when the cell is being added as a linked
-- |   cell from another, passing through the current input port value so the
-- |   current cell can set its state based on that. Used to pull a VarMap
-- |   through for autocomplete purposes, or for the search cell to be able to
-- |   auto-select the parent cell's result set as the resource, etc.
-- | - `NotifyRunCell` allows the cell to notify the notebook that it should be
-- |   run - the cell cannot run itself directly.
data CellEvalQuery a
  = EvalCell CellEvalInput (CellEvalResult → a)
  | SetupCell CellSetupInfo a
  | NotifyRunCell a
  | SetCanceler (Canceler SlamDataEffects) a
  | Save (Json → a)
  | Load Json a

-- | The result value produced when evaluating a cell.
-- |
-- | - `output` is the value that this cell component produces that is taken as
-- |   the input for dependant cells. Not every cell produces an output.
-- | - `messages` is for any error or status messages that arise during
-- |   evaluation. `Left` values are errors, `Right` values are informational
-- |   messages.
type CellEvalResultP a =
  { output ∷ Maybe a
  , messages ∷ Array (Either String String)
  }

type CellEvalResult = CellEvalResultP Port

type CellEvalTP m = ET.ExceptT String (WT.WriterT (Array String) m)
newtype CellEvalT m a = CellEvalT (CellEvalTP m a)

getCellEvalT ∷ ∀ m a. CellEvalT m a → CellEvalTP m a
getCellEvalT (CellEvalT m) = m

instance functorCellEvalT ∷ (Functor m) ⇒ Functor (CellEvalT m) where
  map f = getCellEvalT ⋙ map f ⋙ CellEvalT

instance applyCellEvalT ∷ (Apply m) ⇒ Apply (CellEvalT m) where
  apply (CellEvalT f) = getCellEvalT ⋙ apply f ⋙ CellEvalT

instance applicativeCellEvalT ∷ (Applicative m) ⇒ Applicative (CellEvalT m) where
  pure = pure ⋙ CellEvalT

instance bindCellEvalT ∷ (Monad m) ⇒ Bind (CellEvalT m) where
  bind (CellEvalT m) = (⋙ getCellEvalT) ⋙ bind m ⋙ CellEvalT

instance monadCellEvalT ∷ (Monad m) ⇒ Monad (CellEvalT m)

instance monadTransCellEvalT ∷ MonadTrans CellEvalT where
  lift = lift ⋙ lift ⋙ CellEvalT

instance monadWriterCellEvalT ∷ (Monad m) ⇒ WC.MonadWriter (Array String) (CellEvalT m) where
  writer = WC.writer ⋙ lift ⋙ CellEvalT
  listen = getCellEvalT ⋙ WC.listen ⋙ CellEvalT
  pass = getCellEvalT ⋙ WC.pass ⋙ CellEvalT

instance monadErrorCellEvalT ∷ (Monad m) ⇒ EC.MonadError String (CellEvalT m) where
  throwError = EC.throwError ⋙ CellEvalT
  catchError (CellEvalT m) = CellEvalT ∘ EC.catchError m ∘ (⋙ getCellEvalT)

runCellEvalT
  ∷ ∀ m a
  . (Functor m)
  ⇒ CellEvalT m a
  → m (CellEvalResultP a)
runCellEvalT (CellEvalT m) =
  WT.runWriterT (ET.runExceptT m) <#> uncurry \r ms →
    { output: either (const Nothing) Just r
    , messages: either (Left ⋙ pure) (const []) r ⊕ map Right ms
    }


liftWithCancelerP
  ∷ ∀ a state slot innerQuery innerState
  . Slam a
  → ParentDSL
      state innerState
      CellEvalQuery innerQuery
      Slam slot a
liftWithCancelerP =
  Hu.liftWithCanceler' SetCanceler

liftWithCancelerP'
  ∷ ∀ a state innerState innerQuery query slot
  . Slam a
  → ParentDSL
      state innerState
      (Coproduct CellEvalQuery query) innerQuery
      Slam slot a
liftWithCancelerP' =
  Hu.liftWithCanceler' (\c u → left $ SetCanceler c u)

liftWithCanceler
  ∷ ∀ a state
  . Slam a
  → ComponentDSL state CellEvalQuery Slam a
liftWithCanceler =
  Hu.liftWithCanceler SetCanceler

liftWithCanceler'
  ∷ ∀ state query a
  . Slam a
  → ComponentDSL state (Coproduct CellEvalQuery query) Slam a
liftWithCanceler' =
  Hu.liftWithCanceler (\c u → left $ SetCanceler c u)
