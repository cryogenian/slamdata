module SlamData.Dialog.Share.RotarySelector where

import Prelude

import Data.Functor (($>))
import Data.Functor.Eff (liftEff)
import Data.Lens (LensP(), lens, (%~), (.~), (?~))
import Data.Maybe as M

import CSS.Geometry (height, width, left, marginLeft)
import CSS.Size (px)

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Events.Handler as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B
import Halogen.HTML.CSS.Indexed as CSS

import DOM.HTML (window)
import DOM.HTML.Window as Win
import DOM.HTML.Types as Ht
import DOM.Event.EventTarget as Etr
import DOM.Event.EventTypes as Etp

import SlamData.Effects (Slam())

-- TODO: switch to tuple, since it has ring instances already
type Position =
  {
    x :: Number
  , y :: Number
  }

positionFromMouseEvent
  :: forall r
   . {clientX :: Number, clientY :: Number |r}
  -> Position
positionFromMouseEvent {clientX, clientY} =
  {
    x: clientX
  , y: clientY
  }

type State =
  {
    initialPosition :: M.Maybe Position
  , currentPosition :: M.Maybe Position
  , lastPosition :: M.Maybe Position
  }

initialState :: State
initialState =
  {
    initialPosition: M.Nothing
  , currentPosition: M.Nothing
  , lastPosition: M.Nothing
  }

_initialPosition :: forall a r. LensP {initialPosition :: a |r} a
_initialPosition = lens _.initialPosition _{initialPosition = _}

_currentPosition :: forall a r. LensP {currentPosition :: a |r} a
_currentPosition = lens _.currentPosition _{currentPosition = _}

_lastPosition :: forall a r. LensP {lastPosition :: a|r} a
_lastPosition = lens _.lastPosition _{lastPosition = _}

data Query a
  = Init a
  | StartDragging Position a
  | StopDragging Position a
  | DragCoords Position a

type RotarySelectorDSL = ComponentDSL State Query Slam

comp :: Component State Query Slam
comp = component render eval

render :: State -> ComponentHTML Query
render state =
  H.div
    [
      E.onMouseDown (\evt ->
                      E.preventDefault
                      $> (action $ StartDragging $ positionFromMouseEvent evt))
    , P.initializer (\_ -> action Init)
    ]
    [
      printState state
    ]

-- TODO: remove this crap
printState :: State -> ComponentHTML Query
printState state =
  H.div
    [ CSS.style
        $ marginLeft
        $ px
        $ add (M.fromMaybe zero (state.lastPosition <#> _.x))
        $ M.fromMaybe zero
        $ sub
          (state.currentPosition <#> _.x)
          (state.initialPosition <#> _.x)
    ]
    [
      H.p_ [ H.text $ show
             $ sub
               (state.currentPosition <#> _.x)
               (state.initialPosition <#> _.x)
           ]
    , H.p_ [ H.text $ show $ state.initialPosition <#> _.x ]
    , H.p_ [ H.text $ show $ state.initialPosition <#> _.y ]
    , H.p_ [ H.text $ show $ state.lastPosition <#> _.x ]
    , H.p_ [ H.text $ show $ state.lastPosition <#> _.x ]
    , H.p_ [ H.text $ show $ state.currentPosition <#> _.x ]
    , H.p_ [ H.text $ show $ state.currentPosition <#> _.y ]
    ]

eval :: Natural Query RotarySelectorDSL
eval (StartDragging pos next) = modify (_initialPosition ?~ pos) $> next
eval (StopDragging pos next) = do
  state <- get
  let
    lp =
      M.fromMaybe {x: 0.0, y: 0.0} state.lastPosition
    diff =
      M.fromMaybe {x: 0.0, y: 0.0}
      $ {x: _, y: _}
      <$> ((state.currentPosition <#> _.x) - (state.initialPosition <#> _.x))
      <*> ((state.currentPosition <#> _.y) - (state.initialPosition <#> _.y))
    new =
      { x: lp.x + diff.x, y: lp.y + diff.y }
  modify (_initialPosition .~ M.Nothing)
  modify (_lastPosition ?~ new)
  pure next
eval (Init next) = do
  docTarget <-
    liftEff
    $ window
    >>= Win.document
    <#> Ht.htmlDocumentToEventTarget
  let
    attachMouseUp f =
      Etr.addEventListener Etp.mouseup (Etr.eventListener f) false docTarget
    attachMouseMove f =
      Etr.addEventListener Etp.mousemove (Etr.eventListener f) false docTarget
    handleMouseUp e =
      let
        -- That's safe, mousemove can't generate something w/o clientX/Y
        evt :: { clientX :: Number, clientY :: Number }
        evt = Unsafe.Coerce.unsafeCoerce e
      in
       pure $ action $ StopDragging $ positionFromMouseEvent evt
    handleMouseMove e =
      let
        -- That's safe, mousemove can't generate something w/o clientX/Y
        evt :: { clientX :: Number, clientY :: Number }
        evt = Unsafe.Coerce.unsafeCoerce e
      in
       pure $ action $ DragCoords $ positionFromMouseEvent evt
  subscribe $ eventSource attachMouseUp handleMouseUp
  subscribe $ eventSource attachMouseMove handleMouseMove
  pure next
eval (DragCoords pos next) = modify (_currentPosition ?~ pos) $> next
