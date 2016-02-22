module SlamData.Dialog.Share.RotarySelector.Component.State where

import Prelude

import Data.Maybe as M
import Data.Lens (LensP(), lens)
import Data.ExistsR as Er
import Data.NonEmpty (NonEmpty(), (:|))

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
  | Animating Number Number

newtype Option r = Option {label :: String |r}
runOption :: forall r. Option r -> {label :: String|r}
runOption (Option r) = r

type OptionR = Er.ExistsR Option

type State =
  {
    visualState :: VisualState
  , styles :: CSS
  , element :: M.Maybe Ht.HTMLElement
  , position :: Number
  , key :: M.Maybe String
  , items :: NonEmpty Array OptionR
  , displayedItems :: NonEmpty Array OptionR
  , constStyles :: CSS
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

_items :: forall a r. LensP {items :: a|r} a
_items = lens _.items _{items = _}

_displayedItems :: forall a r. LensP {displayedItems :: a|r} a
_displayedItems = lens _.displayedItems _{displayedItems = _}

_constStyles :: forall a r. LensP {constStyles :: a|r} a
_constStyles = lens _.constStyles _{constStyles = _}

isDragged :: State -> Boolean
isDragged {visualState = Dragging _ } = true
isDragged _ = false

updateStyles
  :: State -> State
updateStyles st@{visualState = Staying, position} =
  st { styles = marginLeft $ px position }
updateStyles st@{visualState = Dragging startedAt, position} =
  st { styles = marginLeft $ px $ position - startedAt }
updateStyles st@{visualState = Animating from to, position, key = M.Just key} =
  st { styles = styles }
  where
  styles = do
    let
      fromStyle = marginLeft $ px from
      toStyle = marginLeft $ px to
    keyframesFromTo key fromStyle toStyle
    animation
      (fromString key)
      (sec $ Math.abs $ (to - from) / 200.0)
      easeOut
      (sec zero)
      (iterationCount one)
      normalAnimationDirection
      forwards
updateStyles s = s

initialState :: forall r. NonEmpty Array (Option r) -> State
initialState items =
  {
    visualState: Staying
  , styles: pure unit
  , element: M.Nothing
  , position: zero
  , key: M.Nothing
  , items: existArr items
  , displayedItems: existArr items
  , constStyles: pure unit
  }
  where
  existArr :: forall r f. (Functor f) => f (Option r) -> f OptionR
  existArr = Unsafe.Coerce.unsafeCoerce
