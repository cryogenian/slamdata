module Halide where

import Halogen
import Halogen.Signal
import Halogen.HTML (HTML(..), div, p_, text, a)
import Halogen.HTML.Attributes
import Halogen.Mixin.UndoRedo
import Halogen.HTML.Events
import Halogen.Themes.Bootstrap3

import qualified Halide.Logo as Logo

data Input = Inc | Dec


data State = State Number

ui :: SF1 Input (HTML Input)
ui = view <$> stateful (State 0) updateState

view :: State -> HTML Input 
view (State count) =
  div (classes [container]) [
    p_ [text (show count)],
    a ((onclick (const $ pure Inc)) <> href "javascript:void(0);") [text "Increase"],
    a ((onclick (const $ pure Dec)) <> href "javascript:void(0);") [text "Decrease"],
    logo unit
    ]
  where logo = Logo.view

updateState :: State -> Input -> State
updateState (State count) Inc = State (count + 1)
updateState (State count) Dec = State (count - 1)


