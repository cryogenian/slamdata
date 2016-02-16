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

module SlamData.FileSystem.Search.Component where

import Prelude

import Control.Monad.Aff (Canceler(), Aff(), cancel, forkAff, later')
import Control.Monad.Eff.Exception (error)
import Control.MonadPlus (guard)
import Control.UI.Browser (setLocation)

import Data.Either (Either(..))
import Data.Functor.Aff (liftAff)
import Data.Functor.Eff (liftEff)
import Data.Lens (lens, LensP(), (^.), (.~), (%~))
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (mempty)
import Data.Path.Pathy (printPath, rootDir)
import Data.These (theseLeft, thisOrBoth, theseRight, these, These(..))

import Halogen (Component(), Eval(), ComponentHTML(), component, modify, get)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import SlamData.Config as Config
import SlamData.Effects (SlamDataEffects())
import SlamData.Effects (Slam())
import SlamData.FileSystem.Listing.Sort (Sort())
import SlamData.FileSystem.Routing (browseURL)
import SlamData.FileSystem.Routing.Salt (newSalt, Salt())
import SlamData.Render.Common
import SlamData.Render.CSS as Rc

import Text.SlamSearch (mkQuery)

import Utils.Path (DirPath())

newtype State = State SearchRec

type SearchRec =
  { valid :: Boolean
  , focused :: Boolean
    -- `These` to differentiate path and search path
  , value :: These String String
  , loading :: Boolean
  , timeout :: Canceler SlamDataEffects
  , path :: DirPath
  , sort :: Sort
  , salt :: Salt
  }

initialState :: Sort -> Salt -> State
initialState sort salt =
  State { valid: true
         , value: This ""
         , focused: false
         , loading: true
         , timeout: mempty
         , path: rootDir
         , sort: sort
         , salt: salt
         }

_State :: LensP State SearchRec
_State  = lens (\(State obj) -> obj) (const State)

_valid :: LensP State Boolean
_valid = _State <<< lens _.valid _{valid = _}

_focused :: LensP State Boolean
_focused = _State <<< lens _.focused _{focused = _}

_value :: LensP State (These String String)
_value = _State <<< lens _.value _{value = _}

_loading :: LensP State Boolean
_loading = _State <<< lens _.loading _{loading = _}

_timeout :: LensP State (Canceler SlamDataEffects)
_timeout = _State <<< lens _.timeout _{timeout = _}

_path :: LensP State DirPath
_path = _State <<< lens _.path _{path = _}

_sort :: LensP State Sort
_sort = _State <<< lens _.sort _{sort = _}

_salt :: LensP State Salt
_salt = _State <<< lens _.salt _{salt = _}

data Query a
  = Focus Boolean a
  | Typed String a
  | Clear a
  | Submit a
  | GetValue (Maybe String -> a)
  | SetLoading Boolean a
  | SetValue (These String String) a
  | SetValid Boolean a
  | IsSearching (Boolean -> a)


comp :: Component State Query Slam
comp = component render eval

render :: State -> ComponentHTML Query
render state =
  H.div [ P.classes [ Rc.search ] ]
  [ H.form [ E.onSubmit (E.input_ Submit)]
    [ H.div [ P.classes searchClasses ]
      [ H.input [ P.classes [ B.formControl ]
                , P.value value
                , E.onFocus (E.input_ (Focus true))
                , E.onBlur (E.input_ (Focus false))
                , E.onValueInput (E.input Typed)
                ]
      , H.span [ P.class_ $ if state ^. _focused
                            then Rc.searchPathActive
                            else Rc.searchPath
               ]
        [ H.span [ P.class_ Rc.searchPathBody ]
          [ H.text value ]
        , H.span [ P.class_ $ if value == ""
                              then Rc.searchAffixEmpty
                              else Rc.searchAffix
                 ]
          [ H.text $ "path:" <> printPath (state ^. _path) ]
        ]
      , H.img [ P.class_ Rc.searchClear
              , P.src searchIcon
              , E.onClick (E.input_ Clear)
              ]
      , H.span [ P.class_ B.inputGroupBtn ]
        [ H.button [ P.classes [ B.btn, B.btnDefault ]
                   , P.enabled (state ^. _valid)
                   ]
          [ glyph B.glyphiconSearch ]
        ]
      ]
    ]
  ]
  where
  searchClasses :: Array H.ClassName
  searchClasses =
    [ B.inputGroup, Rc.searchInput] <> do
      guard (not $ state ^. _valid)
      pure B.hasError

  value = these id id (\x y -> if x == "" then y else x) (state ^. _value)
  searchIcon = if state ^. _loading
               then "img/spin.gif"
               else "img/remove.svg"

eval :: Eval Query State Query Slam
eval (Focus bool next) = do
  modify (_focused .~ bool)
  pure next
eval (Clear next) = do
  -- for peeking in parent
  pure next
eval (Typed str next) = do
  state <- get
  let c = state ^. _timeout
  liftAff $ cancel c (error "timeout")
  c' <- liftAff $ forkAff
        $ later' Config.searchTimeout $ submit
        $ (state # _value %~ (thisOrBoth str <<< theseRight))
  modify $ _value %~ (thisOrBoth str <<< theseRight)
  modify $ _timeout .~ c'
  pure next
eval (Submit next) = do
  state <- get
  mbFn <- liftAff $ submit state
  case mbFn of
    Nothing -> pure unit
    Just fn -> modify fn
  pure next
eval (GetValue continue) = do
  state <- get
  pure $ continue $ theseRight (state ^. _value)
eval (SetLoading bool next) = do
  modify (_loading .~ bool)
  pure next
eval (SetValue tv next) = do
  modify (_value .~ tv)
  pure next
eval (SetValid bool next) = do
  modify (_valid .~ bool)
  pure next
eval (IsSearching continue) = do
  state <- get
  pure $ continue $ isJust $ theseRight $ state ^. _value

submit :: State -> Aff SlamDataEffects (Maybe (State -> State))
submit state = do
  salt <- liftEff newSalt
  case theseLeft (state ^. _value) of
    Just q -> case mkQuery q of
      Left _ | q /= "" -> pure $ pure (_valid .~ false)
      _ -> do
        liftEff $ setLocation
          $ browseURL (Just q) (state ^. _sort) salt (state ^. _path)
        pure Nothing
    _ -> pure  Nothing
