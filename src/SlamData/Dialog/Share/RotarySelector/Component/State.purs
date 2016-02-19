module SlamData.Dialog.Share.RotarySelector.Component.State where

import Prelude

import Data.Maybe as M
import Data.Lens (LensP(), lens)

import DOM.HTML.Types as Ht

import CSS.Geometry (height, width, left, marginLeft)
import CSS.Size (px)
import CSS.Stylesheet (CSS(), keyframesFromTo, (?))
import CSS.Animation (animation, iterationCount, normalAnimationDirection, forwards)
import CSS.Time (sec)
import CSS.Transition (easeOut)
import CSS.String (fromString)
import CSS.Render as Cr
import CSS.Selector (Selector(..), Predicate(AttrVal), Path(..), Refinement(..))

import SlamData.Effects (Slam())

data VisualState
  = Staying
  | Dragging Number
  | AnimatedTo Number

type State =
  {
    visualState :: VisualState
  , styles :: CSS
  , element :: M.Maybe Ht.HTMLElement
  , position :: Number
  , key :: M.Maybe String
  }

_visualState :: forall a r. LensP {visualState :: a|r} a
_visualState = lens _.visualState _{visualState = _}

_styles :: forall a r. LensP {styles :: a|r} a
_styles = lens _.styles _{styles = _}

_element :: forall a r. LensP {element :: a|r} a
_element = lens _.element _{element = _}

_position :: forall a r. LensP {position :: a|r} a
_position = lens _.position _{position = _}

_key :: forall a r. LensP {key :: a|r} a
_key = lens _.key _{key = _}

updateStyles :: State -> State
updateStyles st@{visualState = Dragging startedAt, position} =
  st { styles = marginLeft $ px $ position - startedAt }
updateStyles s = s

isDragged :: State -> Boolean
isDragged {visualState = Dragging _} = true
isDragged _ = false

initialState :: State
initialState =
  {
    visualState: Staying
  , styles: pure unit
  , element: M.Nothing
  , position: zero
  , key: M.Nothing
  }
