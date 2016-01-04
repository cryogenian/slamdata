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

module Form.SelectPair.Component where

import Prelude

import Control.Monad.Aff (Aff())

import Data.Array (null, singleton)
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct())
import Data.Lens (LensP(), lens, (^.), (.~), (%~), view)
import Data.Maybe (Maybe(), maybe)
import Data.Tuple (Tuple(..))
import Form.Select.Component (Query(..), select)

import Halogen
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import Render.CssClasses as Rc
import Model.Select
import Utils.Array (enumerate)

type Slam e = Aff (HalogenEffects e)

type State a =
  { model :: Select a
  , disabled :: Boolean
  , opened :: Boolean
  }

initialState :: forall a. Select a -> State a
initialState sel =
  { model: sel
  , disabled: false
  , opened: false
  }

_model :: forall a r. LensP {model :: a | r} a
_model = lens _.model _{model = _}

_disabled :: forall a r. LensP {disabled :: a | r} a
_disabled = lens _.disabled _{disabled = _}

_opened :: forall a r. LensP {opened :: a | r} a
_opened = lens _.opened _{opened = _}

type StateP a b e =
  InstalledState (State a) (Select b) (Query a) (Query b) (Slam e) Unit

type QueryP a b = Coproduct (Query a) (ChildF Unit (Query b))

type PairConfig a r =
  { disableWhen :: Int -> Boolean
  , defaultWhen :: Int -> Boolean
  , ariaLabel :: Maybe String
  , mainState :: Select a
  , classes :: Array H.ClassName
  | r
  }


selectPair
  :: forall a b e r
   . (OptionVal a, OptionVal b)
  => PairConfig b r -> Component (StateP a b e) (QueryP a b) (Slam e)
selectPair config =
  parentComponent' (render config) eval peek

render
  :: forall a b e r
   . (OptionVal a, OptionVal b)
  => PairConfig b r
  -> RenderParent (State a) (Select b) (Query a) (Query b) (Slam e) Unit
render config state = H.div_
  [ H.slot unit \_ -> { component: select config
                      , initialState: config.mainState
                      }

  , H.button [ P.classes ([ B.formControl ] <> config.classes <> clsValOrEmpty )
             , P.disabled state.disabled
             , E.onClick (E.input_ ToggleOpened)
             ] [ ]
  , H.ul [ P.classes $ [ B.listGroup
                       , B.fade
                       , Rc.fileListGroup
                       , Rc.aggregation ]
                     <> if state.opened then [ B.in_ ] else [ ]
         ]
    $ map option $ enumerate $ view _options state.model
  ]
  where
  clsValOrEmpty :: Array H.ClassName
  clsValOrEmpty =
    map H.className $ maybe [] singleton $ map stringVal $ view _value state.model

  option
    :: Tuple Int a
    -> ParentHTML (Select b) (Query a) (Query b) (Slam e) Unit
  option (Tuple i val) =
    H.button [ P.classes [ B.listGroupItem ]
             , E.onClick (E.input_ (Choose i))
             ]
    [ H.text $ stringVal val ]

eval
  :: forall a b e
   . (Eq a, Eq b)
  => EvalParent (Query a) (State a) (Select b) (Query a) (Query b) (Slam e) Unit
eval (Choose i next) = do
  modify (_opened .~ false)
  modify (_model %~ trySelect i)
  pure next
eval (SetSelect sel next) = modify (_model .~ sel) $> next
eval (GetValue continue) = map continue $ gets $ view $ _model <<< _value
eval (GetSelect continue) = map continue $ gets $ view _model
eval (ToggleOpened next) = modify (_opened %~ not) $> next


peek
  :: forall a b e
   . (OptionVal a, OptionVal b)
  => Peek (ChildF Unit (Query b))
   (State a) (Select b) (Query a) (Query b) (Slam e) Unit
peek (ChildF _ (SetSelect s _)) = modify (_disabled .~ null (s ^. _options))
peek _ = pure unit
