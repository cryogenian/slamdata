module Utils.Aff where

import Prelude
import Control.Monad.Aff
import Control.Monad.Aff.AVar
import Control.Coroutine
import Control.Coroutine.Aff
import Control.Coroutine.Stalling as SCR
import Data.Either
import Halogen.Query.EventSource

import Unsafe.Coerce

eS :: forall f e. (Unit -> f Unit) -> EventSource f (Aff (avar :: AVAR|e))
eS f =
  EventSource $ SCR.producerToStallingProducer $ produce \emit -> do
    emit $ Left $ f unit
    emit $ Right unit

liftWithCanceler
  :: forall a e f
   . (Unit -> f Unit)
  -> Aff e a
  ->
