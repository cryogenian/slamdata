module Utils.Debounced where

import Prelude

import Control.Bind ((=<<))
import Control.Coroutine.Aff (produce)
import Control.Coroutine.Stalling (producerToStallingProducer)
import Control.Monad.Aff (Aff(), forkAff, later', cancel)
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Ref (REF(), newRef, readRef, writeRef)

import Data.Either (Either(..))
import Data.Functor.Eff (liftEff)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.NaturalTransformation (Natural())
import Data.Time (Milliseconds(..))

import DOM.Timer (Timer(), timeout, clearTimeout)

import Halogen.Query.EventSource (EventSource(..))

type DebounceEffects eff = (ref :: REF | eff)

-- | Sets up a debounced `Aff` action.
-- |
-- | The milliseconds value is the time to wait before triggering an action. The
-- | result is a function that when passed an `Aff` action will cancel any
-- | previously-passed pending action for which the delay has not yet elapsed,
-- | and then start the delay for the newly provided action.
debouncedAff
  :: forall eff a
   . Milliseconds
  -> Aff (DebounceEffects eff) (Aff (DebounceEffects eff) a -> Aff (DebounceEffects eff) Unit)
debouncedAff (Milliseconds ms) = do
  ref <- liftEff (newRef Nothing)
  pure \act -> do
    maybe (pure false) (`cancel` (error "interrupted")) <$> liftEff (readRef ref)
    canceller <- forkAff $ later' (Int.floor ms) act
    liftEff $ writeRef ref (Just canceller)

-- | Sets up and subscribes to an `EventSource` that will emit values after a
-- | delay.
-- |
-- | The milliseconds is the time to wait before emitting an input value.
-- | The first returned function expects a function to subscribe to the event
-- | source.
-- | The second returned function will trigger values from the
debouncedEventSource
  :: forall f g eff
   . (Monad g)
  => Natural (Eff (ref :: REF, avar :: AVAR, timer :: Timer | eff)) g
  -> (EventSource f (Aff (ref :: REF, avar :: AVAR, timer :: Timer | eff)) -> g Unit)
  -> Milliseconds
  -> g (f Unit -> Aff (ref :: REF, avar :: AVAR, timer :: Timer | eff) Unit)
debouncedEventSource lift subscribe (Milliseconds ms) = do
  timeoutRef <- lift (newRef Nothing)
  emitRef <- lift (newRef Nothing)

  subscribe $ EventSource $ producerToStallingProducer $ produce
    \emit -> writeRef emitRef (Just emit)

  pure \act -> liftEff do
    maybe (pure unit) clearTimeout =<< readRef timeoutRef
    timeoutId <- timeout (Int.floor ms) $
      maybe (pure unit) ($ (Left act)) =<< readRef emitRef
    writeRef timeoutRef (Just timeoutId)
    pure unit
