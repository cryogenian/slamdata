module SlamData.StylesContainer.Component where

import Prelude

import Control.Monad.Aff (Aff())

import Data.Foldable as F
import Data.Functor (($>))

import Halogen
import CSS.Stylesheet (importUrl)
import Halogen.HTML.CSS as CSS

import SlamData.StylesContainer.Model (runStyleURL, StyleURL(..))

type State = Array StyleURL

initialState :: State
initialState = [ ]

data Query a
  = AddStylesheets (Array StyleURL) a
  | SetStylesheets (Array StyleURL) a

comp :: forall e. Component State Query (Aff e)
comp = component render eval
  where
  render :: State -> ComponentHTML Query
  render styles =
    CSS.stylesheet do
      F.for_ styles (runStyleURL >>> importUrl)

  eval :: Natural Query (ComponentDSL State Query (Aff e))
  eval (AddStylesheets ss next) = (modify $ flip append ss) $> next
  eval (SetStylesheets ss next) = set ss $> next
