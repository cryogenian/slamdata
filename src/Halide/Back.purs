module Halide.Back where

import Halogen.HTML (HTML(..), i, a)
import Halogen.HTML.Attributes
import Halogen.HTML.Events

import Model
import qualified Router as R

type State = Mount

data Input = Update State | GoHome

--viewIcon :: State -> HTML Input
viewIcon state =
  case state of
    File -> icon "glyphicon-file" 
    Database -> icon "glypicon-hdd" 
    Notebook -> icon "glypicon-list-alt"
    Directory -> icon "glyphicon-folder-open"
    Table -> icon "glyphicon-th"
  where icon additionalClass =
          i (classes [className "glyphicon", className additionalClass]) []

--view :: State -> HTML Input
view state =
  a (class_ (className "navbar-brand") <>
     href "javascript:void(0);" <>
     onclick (const $ pure GoHome)) [
    viewIcon state
    ]

