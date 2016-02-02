module Utils.Aff where

import Prelude
import Control.Monad.Aff (Aff(), forkAff)
import Control.Monad.Aff.AVar
import Control.Coroutine
import Control.Coroutine.Aff
import Control.Coroutine.Stalling as SCR
import Data.Either
import Halogen.Query.EventSource
import Data.Functor.Aff
import Halogen (subscribe', ParentDSL())
import Notebook.Common (Slam())

import Unsafe.Coerce
import Notebook.Cell.Common.EvalQuery

eS :: forall f e. (Unit -> f Unit) -> EventSource f (Aff (avar :: AVAR|e))
eS f =
  EventSource $ SCR.producerToStallingProducer $ produce \emit -> do
    emit $ Left $ f unit
    emit $ Right unit

liftWithCanceler
  :: forall a e slot state innerQuery
   . Slam a
  -> ParentDSL slot state CellEvalQuery innerQuery Slam Unit a
liftWithCanceler aff = do
  v <- liftAff makeVar
  canceler <- liftAff $ forkAff do
    res <- aff
    putVar v res
  subscribe'
    $ EventSource
    $ SCR.producerToStallingProducer
    $ produce \emit -> do
      emit $ Left $ AddCanceler canceler unit
      emit $ Right unit
  liftAff $ takeVar v

{-

produce_
  :: forall a r f eff
   . (FunctorAff (avar :: AVAR | eff) f)
  => ((Either a r -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) Unit)
  -> Producer a f r
produce_ = hoistFreeT liftAff <<< produce

produceCancelable
  :: forall a r f eff
   . (FunctorAff (avar :: AVAR|eff) f)
  => Aff (avar :: AVAR|eff) r
  -> Producer (Canceler (avar :: AVAR|eff)) f r
produceCancelable aff = produce_ \emit ->
  runAff (const $ pure unit) pure do
    canceler <- liftAff $ forkAff do
      res <- aff
      liftEff $ emit $ Right res
    liftEff $ emit $ Left canceler

consumeCancelable
  :: forall a r f eff
   . (Monad f, FunctorAff (avar :: AVAR|eff) f)
  => (Canceler eff -> f Unit)
  -> Consumer (Canceler eff) f a
consumeCancelable eatCanceler = consumer \s -> eatCanceler s $> Nothing


liftAffWithCanceler
  :: forall a eff f
   . (MonadRec f, FunctorAff (avar :: AVAR|eff) f)
  => (Canceler (avar :: AVAR|eff) -> f Unit)
  -> Aff (avar :: AVAR|eff) a
  -> f a
liftAffWithCanceler eatCanceler aff =
  runProcess ((produceCancelable aff) $$ (consumeCancelable eatCanceler))
-}
