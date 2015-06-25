module Input.File
  ( Input()
  , FileInput(..)
  , updateState
  ) where

import Control.Alt ((<|>))
import Data.Either (Either())
import Data.Inject1 (prj)
import Data.Maybe.Unsafe (fromJust)
import Input.File.Item (ItemInput(), inputItem)
import Input.File.Mount (MountInput(), inputMount)
import Model.File (State(), _items, _dialog, _sort, isSearching)
import Optic.Core ((..), (^.), (%~))
import Optic.Refractor.Prism (_Just)

type Input = Either ItemInput (Either FileInput MountInput)

data FileInput = WithState (State -> State)

updateState :: State -> Input -> State
updateState state input =
  fromJust $ (inputFile state <$> prj input)
         <|> ((\i -> state # _items %~ inputItem (state ^. _sort) (isSearching state) i) <$> prj input)
         <|> ((\i -> state # _dialog .. _Just %~ inputMount i) <$> prj input)

inputFile :: State -> FileInput -> State
inputFile state (WithState f) = f state
