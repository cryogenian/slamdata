module Node.ChildProcess where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (Error)

import Data.Function (Fn2, Fn6, runFn2, runFn6)
import Data.Maybe (Maybe(..))

import Node.Stream (Readable)
import Unsafe.Coerce (unsafeCoerce)

foreign import data ChildProcess :: *
foreign import data CHILD_PROCESS :: !
foreign import data SpawnOption :: *
foreign import emptySpawnOptions :: SpawnOption


foreign import stdoutImpl
  :: forall e r
   . ChildProcess
  -> Eff (childProcess :: CHILD_PROCESS | e)
       (Readable r (childProcess :: CHILD_PROCESS | e))

foreign import stderrImpl
  :: forall e r
   . ChildProcess
  -> Eff (childProcess :: CHILD_PROCESS | e)
       (Readable r (childProcess :: CHILD_PROCESS | e))

foreign import stdinImpl
  :: forall e r
   . ChildProcess
  -> Eff (childProcess :: CHILD_PROCESS | e)
       (Readable r (childProcess :: CHILD_PROCESS | e))

stdout
  :: forall e r m
   . (MonadEff (childProcess :: CHILD_PROCESS | e) m)
  => ChildProcess -> m (Readable r (childProcess :: CHILD_PROCESS | e))
stdout = liftEff <<< stdoutImpl

stderr
  :: forall e r m
   . (MonadEff (childProcess :: CHILD_PROCESS | e) m)
  => ChildProcess -> m (Readable r (childProcess :: CHILD_PROCESS | e))
stderr = liftEff <<< stderrImpl

stdin
  :: forall e r m
   . (MonadEff (childProcess :: CHILD_PROCESS | e) m)
  => ChildProcess -> m (Readable r (childProcess :: CHILD_PROCESS | e))
stdin = liftEff <<< stdinImpl

foreign import spawnImpl
  :: forall e a
   . Fn6 String (Array String) SpawnOption
       (Maybe Error -> Eff (childProcess :: CHILD_PROCESS|e) Unit)
       (a -> Maybe a)
       (Maybe a)
       (Eff (childProcess :: CHILD_PROCESS|e) ChildProcess)

spawn
  :: forall e m
   . (MonadEff (childProcess :: CHILD_PROCESS|e) m)
  => String -> Array String
  -> (Maybe Error -> Eff (childProcess :: CHILD_PROCESS|e) Unit)
  -> m ChildProcess
spawn name args handler =
  liftEff $ runFn6 spawnImpl name args emptySpawnOptions handler Just Nothing

foreign import execSyncImpl
  :: forall e
   . Fn2 String SpawnOption (Eff (childProcess :: CHILD_PROCESS|e) Unit)

execSync
  :: forall e m
   . (MonadEff (childProcess :: CHILD_PROCESS |e) m)
  => String -> SpawnOption -> m Unit
execSync name opts = liftEff $ runFn2 execSyncImpl name opts


foreign import killImpl
  :: forall e. ChildProcess -> Eff (childProcess :: CHILD_PROCESS|e) Unit

kill
  :: forall e m. (MonadEff (childProcess :: CHILD_PROCESS|e) m)
  => ChildProcess -> m Unit
kill cp = liftEff $ killImpl cp

makeSpawnOption :: { stdio :: Array String } -> SpawnOption
makeSpawnOption = unsafeCoerce
