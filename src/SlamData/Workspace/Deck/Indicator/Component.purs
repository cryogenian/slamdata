module SlamData.Workspace.Deck.Indicator.Component where

import SlamData.Prelude

import Data.Array as Arr
import Data.Lens as Lens
import Data.Lens (lens, LensP, (.~))

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Effects (Slam)
import SlamData.Workspace.Card.CardType (CardType(..))

data Query a
  = UpdatePortList (Array CardType) a
  | UpdateActiveId Int a

data Status
  = Available
  | Errored
  | Placeholder
  | Pending

type State =
  { icons ∷ Array Status
  , focused ∷ Int
  }

_icons ∷ ∀ a r. LensP {icons ∷ a|r} a
_icons = lens (_.icons) (_{icons = _})

_focused ∷ ∀ a r. LensP {focused ∷ a |r} a
_focused = lens (_.focused) (_{focused = _})

initialState ∷ State
initialState =
  { focused: top
  , icons: [ ]
  }

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Slam

comp ∷ H.Component State Query Slam
comp = H.component {render, eval}

render ∷ State → HTML
render state =
  HH.div [HP.classes [ HH.className "indicator" ] ]
    $ foldMap renderCircle
       $ Arr.zip (Arr.range 0 (Arr.length state.icons)) state.icons
  where
  renderCircle ∷ Int × Status → Array HTML
  renderCircle (ix × status) =
    [ HH.i
      [ HP.classes $
          pure case status of
            Available → HH.className "available"
            Errored → HH.className "errored"
            Placeholder → HH.className "placeholder"
            Pending → HH.className "pending"

        ⊕ ((guard (state.focused ≡ ix)) $> (HH.className "focused"))
       ]

      [ HH.text "" ]
    ]

  notFocused ∷ Boolean
  notFocused | state.focused < 0 = true
  notFocused | state.focused >= Arr.length state.icons = true
  notFocused = false

eval ∷ Query ~> DSL
eval (UpdatePortList cardTypes next) = do
  H.modify $ Lens.set _icons $ cardTypeToStatus <$> cardTypes
  pure next
  where

  cardTypeToStatus =
    case _ of
      ErrorCard → Errored
      NextAction → Placeholder
      PendingCard → Pending
      _ → Available

eval (UpdateActiveId ix next) = do
  H.modify (_focused .~ ix)
  pure next
