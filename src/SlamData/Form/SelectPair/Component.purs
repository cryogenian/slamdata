{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Form.SelectPair.Component where

import SlamData.Prelude

import Data.Array (null, singleton)
import Data.Lens (LensP, lens, (^.), (.~), (%~), view)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Form.Select as S
import SlamData.Form.Select.Component (Query(..), select)
import SlamData.Render.CSS as Rc
import SlamData.Effects (Slam)

import Utils.Array (enumerate)

type State a =
  { model :: S.Select a
  , disabled :: Boolean
  , opened :: Boolean
  }

initialState :: forall a. S.Select a -> State a
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

type StateP a b =
  H.ParentState (State a) (S.Select b) (Query a) (Query b) Slam Unit

type QueryP a b = Coproduct (Query a) (H.ChildF Unit (Query b))

type PairConfig a r =
  { disableWhen :: Int -> Boolean
  , defaultWhen :: Int -> Boolean
  , ariaLabel :: Maybe String
  , mainState :: S.Select a
  , classes :: Array HH.ClassName
  | r
  }

type HTML a b = H.ParentHTML (S.Select b) (Query a) (Query b) Slam Unit
type DSL a b = H.ParentDSL (State a) (S.Select b) (Query a) (Query b) Slam Unit

selectPair
  :: forall a b r
   . (S.OptionVal a, S.OptionVal b)
  => PairConfig b r
  -> H.Component (StateP a b) (QueryP a b) Slam
selectPair config =
  H.parentComponent
    { render: (render config)
    , eval
    , peek: Just (peek <<< H.runChildF)
    }

render
  :: forall a b r
   . (S.OptionVal a, S.OptionVal b)
  => PairConfig b r
  -> State a
  -> HTML a b
render config state =
  HH.div_
    [ HH.slot unit \_ ->
        { component: select config
        , initialState: config.mainState
        }
    , HH.button
        [ HP.classes ([ B.formControl ] <> config.classes <> clsValOrEmpty )
        , HP.disabled state.disabled
        , HE.onClick (HE.input_ ToggleOpened)
        ] []
    , HH.ul
        [ HP.classes
            $ [ B.listGroup
              , B.fade
              , Rc.fileListGroup
              , Rc.aggregation
              ]
            <> if state.opened then [ B.in_ ] else [ ]
        ]
        $ map option $ enumerate $ view S._options state.model
    ]
  where
  clsValOrEmpty :: Array HH.ClassName
  clsValOrEmpty =
    map HH.className $ maybe [] singleton $ map S.stringVal $ view S._value state.model

  option
    :: Tuple Int a
    -> HTML a b
  option (Tuple i val) =
    HH.button
      [ HP.classes [ B.listGroupItem ]
      , HE.onClick (HE.input_ (Choose i))
      ]
      [ HH.text $ S.stringVal val ]

eval :: forall a b. (Eq a, Eq b) => Query a ~> DSL a b
eval (Choose i next) = do
  H.modify (_opened .~ false)
  H.modify (_model %~ S.trySelect i)
  pure next
eval (SetSelect sel next) = H.modify (_model .~ sel) $> next
eval (GetValue continue) = map continue $ H.gets $ view $ _model <<< S._value
eval (GetSelect continue) = map continue $ H.gets $ view _model
eval (ToggleOpened next) = H.modify (_opened %~ not) $> next

peek
  :: forall a b x
   . (S.OptionVal a, S.OptionVal b)
  => Query b x
  -> DSL a b Unit
peek (SetSelect s _) = H.modify (_disabled .~ null (s ^. S._options))
peek _ = pure unit
