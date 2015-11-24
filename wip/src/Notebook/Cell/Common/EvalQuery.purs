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

module Notebook.Cell.Common.EvalQuery
  ( CellEvalQuery(..)
  , CellEvalResult()
  , CellEvalResultP()
  , CellEvalInputP()
  , CellEvalInputPre()
  , CellEvalInput()
  , CellEvalT()
  , runCellEvalT
  , temporaryOutputResource
  , prepareCellEvalInput
  ) where

import Prelude

import Control.Monad.Error.Class as EC
import Control.Monad.Except.Trans as ET
import Control.Monad.Writer.Class as WC
import Control.Monad.Writer.Trans as WT
import Control.Monad.Trans as MT

import Data.Either as E
import Data.Maybe as M
import Data.Tuple as TPL
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P

import Model.Port (Port())
import Model.CellId (CellId(), cellIdToString)
import Model.Resource as R
import Utils.Path (DirPath())

type CellEvalInputP r =
  { notebookPath :: M.Maybe DirPath
  , inputPort :: M.Maybe Port
  , cellId :: CellId
  | r
  }

type CellEvalInputPre = CellEvalInputP ()
type CellEvalInput =
  CellEvalInputP
    ( cachingEnabled :: M.Maybe Boolean
    )

prepareCellEvalInput
  :: M.Maybe Boolean
  -> CellEvalInputPre
  -> CellEvalInput
prepareCellEvalInput cachingEnabled { notebookPath, inputPort, cellId } =
  { notebookPath
  , inputPort
  , cellId
  , cachingEnabled
  }

temporaryOutputResource
  :: forall r
   . CellEvalInputP r
  -> R.Resource
temporaryOutputResource info =
  R.mkFile $ E.Left $ outputDirectory </> outputFile
  where
    outputDirectory =
      filterMaybe (== P.rootDir) info.notebookPath #
        M.fromMaybe (P.rootDir </> P.dir ".tmp")

    outputFile =
      P.file $ "out" <> cellIdToString info.cellId

    filterMaybe :: forall a. (a -> Boolean) -> M.Maybe a -> M.Maybe a
    filterMaybe p m =
      m >>= \x ->
        if p x then M.Nothing else pure x

-- | The query algebra shared by the inner parts of a cell component.
-- |
-- | - `EvalCell` is a command sent from the notebook that runs the cell. An
-- |   optional input value (the output from another cell) is provided, and a
-- |   continuation for the evaluation result to be returned to.
-- | - `NotifyRunCell` allows the cell to notify the notebook that it should be
-- |   run - the cell cannot run itself directly.
data CellEvalQuery a
  = EvalCell CellEvalInput (CellEvalResult -> a)
  | NotifyRunCell a

-- | The result value produced when evaluating a cell.
-- |
-- | - `output` is the value that this cell component produces that is taken as
-- |   the input for dependant cells. Not every cell produces an output.
-- | - `messages` is for any error or status messages that arise during
-- |   evaluation. `Left` values are errors, `Right` values are informational
-- |   messages.
type CellEvalResultP a =
  { output :: M.Maybe a
  , messages :: Array (E.Either String String)
  }

type CellEvalResult = CellEvalResultP Port

type CellEvalTP m = ET.ExceptT String (WT.WriterT (Array String) m)
newtype CellEvalT m a = CellEvalT (CellEvalTP m a)

getCellEvalT :: forall m a. CellEvalT m a -> CellEvalTP m a
getCellEvalT (CellEvalT m) = m

instance functorCellEvalT :: (Functor m) => Functor (CellEvalT m) where
  map f = getCellEvalT >>> map f >>> CellEvalT

instance applyCellEvalT :: (Apply m) => Apply (CellEvalT m) where
  apply (CellEvalT f) = getCellEvalT >>> apply f >>> CellEvalT

instance applicativeCellEvalT :: (Applicative m) => Applicative (CellEvalT m) where
  pure = pure >>> CellEvalT

instance bindCellEvalT :: (Monad m) => Bind (CellEvalT m) where
  bind (CellEvalT m) = (>>> getCellEvalT) >>> bind m >>> CellEvalT

instance monadCellEvalT :: (Monad m) => Monad (CellEvalT m)

instance monadTransCellEvalT :: MT.MonadTrans CellEvalT where
  lift = MT.lift >>> MT.lift >>> CellEvalT

instance monadWriterCellEvalT :: (Monad m) => WC.MonadWriter (Array String) (CellEvalT m) where
  writer = WC.writer >>> MT.lift >>> CellEvalT
  listen = getCellEvalT >>> WC.listen >>> CellEvalT
  pass = getCellEvalT >>> WC.pass >>> CellEvalT

instance monadErrorCellEvalT :: (Monad m) => EC.MonadError String (CellEvalT m) where
  throwError = EC.throwError >>> CellEvalT
  catchError (CellEvalT m) = CellEvalT <<< EC.catchError m <<< (>>> getCellEvalT)

runCellEvalT
  :: forall m a
   . (Functor m)
  => CellEvalT m a
  -> m (CellEvalResultP a)
runCellEvalT (CellEvalT m) =
  WT.runWriterT (ET.runExceptT m) <#> TPL.uncurry \r ms ->
    { output: E.either (const M.Nothing) M.Just r
    , messages: E.either (E.Left >>> pure) (const []) r <> map E.Right ms
    }
