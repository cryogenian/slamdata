module SlamData.Notebook.Card.Error.Component.State
  ( State
  , initialState
  , _message

  , encode
  , decode
  ) where

import SlamData.Prelude

import Data.Argonaut as JS
import Data.Argonaut ((:=), (~>), (.?))
import Data.Lens (LensP, lens)

type State =
  { message :: Maybe String
  }

initialState ∷ State
initialState =
  { message: Nothing
  }

_message ∷ LensP State (Maybe String)
_message = lens (_.message) (_ { message = _ })

encode
  ∷ State
  → JS.Json
encode s =
  "message" := s.message
    ~> JS.jsonEmptyObject

decode
  ∷ JS.Json
  → Either String State
decode =
  JS.decodeJson >=> \obj → do
    message ← obj .? "message"
    pure { message }
