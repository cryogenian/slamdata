module SlamData.Workspace.Card.SetupFormInput.TextLike.Eval
  ( eval
  , module SlamData.Workspace.Card.SetupFormInput.TextLike.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.Throw (class MonadThrow)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.SetupFormInput.TextLike.Model (Model)

eval
  ∷ ∀ m
  . ( MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    )
  ⇒ Model
  → Port.TaggedResourcePort
  → FormInputType
  → m Port.Port
eval m tr fit = BCE.buildChartEval' buildFn tr m
  where
  -- We need to store axes to display selects
  buildFn axes conf records =
    pure
    $ Port.SetupTextLikeFormInput
        { name: conf.name
        , taggedResource
        , formInputType
        , cursor: conf.value
        }
