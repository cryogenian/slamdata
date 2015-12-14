{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Form.Select.Component where

import Prelude

import Control.Monad.Aff (Aff())

import Data.Array (length, range, zipWith, singleton)
import Data.Functor (($>))
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe.Unsafe (fromJust)

import Halogen
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B
import Halogen.CustomProps.Indexed as Cp

import Model.Select
import Utils (stringToInt)

data Query s a
  = Choose Int a
  | SetSelect (Select s) a
  | GetValue (Maybe s -> a)
  | GetSelect (Select s -> a)

type Slam e = Aff (HalogenEffects e)

type SelectConfig r =
  { disableWhen :: Int -> Boolean
  , defaultWhen :: Int -> Boolean
  , ariaLabel :: Maybe String
  | r
  }

primarySelect
  :: forall a e
   . (OptionVal a)
  => Maybe String -> Component (Select a) (Query a) (Slam e)
primarySelect mbLabel =
  select { disableWhen: (< 2), defaultWhen: (> 1), ariaLabel: mbLabel }

secondarySelect
  :: forall a e
   . (OptionVal a)
  => Maybe String -> Component (Select a) (Query a) (Slam e)
secondarySelect mbLabel =
  select { disableWhen: (< 1), defaultWhen: (const true), ariaLabel: mbLabel }

select
  :: forall a e r
   . (OptionVal a)
  => SelectConfig r -> Component (Select a) (Query a) (Slam e)
select config =
  component (render config) eval

render
  :: forall a r
   . (OptionVal a)
  => SelectConfig r -> Select a -> ComponentHTML (Query a)
render config state =
  H.select ([ P.classes [ B.formControl ]
              -- `fromJust` is safe here because we know that value are `show`n ints
            , E.onValueChange (E.input (Choose <<< fromJust <<< stringToInt))
            , P.disabled $ config.disableWhen len
            ]
           <> maybe [] (singleton <<< Cp.ariaLabel) config.ariaLabel)
  (defOption <> (zipWith (option selected) opts (range 0 len)))
  where
  len :: Int
  len = length opts

  opts :: Array a
  opts = state ^. _options

  selected :: Maybe a
  selected = state ^. _value

  defOption :: Array (ComponentHTML (Query a))
  defOption =
    if config.defaultWhen len
    then singleton $ defaultOption selected
    else [ ]

  defaultOption :: Maybe a -> ComponentHTML (Query a)
  defaultOption val =
    H.option [ P.selected (val == Nothing)
             , P.value "-1"
             ]
    [ H.text "Select axis source" ]

  option :: Maybe a -> a -> Int -> ComponentHTML (Query a)
  option currentVal val i =
    H.option [ P.selected (pure val == currentVal)
             , P.value (show i)
             ]
    [ H.text (stringVal val) ]

eval :: forall a e. (Eq a) => Eval (Query a) (Select a) (Query a) (Slam e)
eval (Choose i next) = modify (trySelect i) $> next
eval (SetSelect s next) = modify (const s) $> next
eval (GetValue continue) = map continue $ gets (^. _value)
eval (GetSelect continue) = map continue get
