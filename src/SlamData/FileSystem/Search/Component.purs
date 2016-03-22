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

import SlamData.Prelude

import Control.Monad.Aff (Canceler, Aff, cancel, forkAff, later')
import Control.Monad.Eff.Exception (error)
import Control.UI.Browser (setLocation)

import Data.Lens (lens, LensP, (.~), (%~))
import Data.Path.Pathy (printPath, rootDir)
import Data.These (theseLeft, thisOrBoth, theseRight, these, These(..))

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Config as Config
import SlamData.Effects (Slam, SlamDataEffects)
import SlamData.FileSystem.Listing.Sort (Sort)
import SlamData.FileSystem.Routing (browseURL)
import SlamData.FileSystem.Routing.Salt (newSalt, Salt)
import SlamData.Render.Common (glyph)
import SlamData.Render.CSS as Rc

import Text.SlamSearch (mkQuery)

import Utils.Path (DirPath)

type State =
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
  { valid: true
  , value: This ""
  , focused: false
  , loading: true
  , timeout: mempty
  , path: rootDir
  , sort: sort
  , salt: salt
  }

_valid :: LensP State Boolean
_valid = lens _.valid _{valid = _}

_focused :: LensP State Boolean
_focused = lens _.focused _{focused = _}

_value :: LensP State (These String String)
_value = lens _.value _{value = _}

_loading :: LensP State Boolean
_loading = lens _.loading _{loading = _}

_timeout :: LensP State (Canceler SlamDataEffects)
_timeout = lens _.timeout _{timeout = _}

_path :: LensP State DirPath
_path = lens _.path _{path = _}

_sort :: LensP State Sort
_sort = lens _.sort _{sort = _}

_salt :: LensP State Salt
_salt = lens _.salt _{salt = _}

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

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Slam

comp :: H.Component State Query Slam
comp = H.component { render, eval }

render :: State -> HTML
render state =
  HH.div
    [ HP.classes [ Rc.search ] ]
    [ HH.form
        [ HE.onSubmit (HE.input_ Submit)]
        [ HH.div
            [ HP.classes searchClasses ]
            [ HH.input
                [ HP.classes [ B.formControl ]
                , HP.value value
                , HE.onFocus (HE.input_ (Focus true))
                , HE.onBlur (HE.input_ (Focus false))
                , HE.onValueInput (HE.input Typed)
                , HP.title "File search field"
                , ARIA.label "File search field"
                ]
            , HH.span
                [ HP.class_
                    if state.focused
                    then Rc.searchPathActive
                    else Rc.searchPath
                ]
                [ HH.span
                    [ HP.class_ Rc.searchPathBody ]
                    [ HH.text value ]
                , HH.span
                    [ HP.class_
                        if value == ""
                        then Rc.searchAffixEmpty
                        else Rc.searchAffix
                    ]
                    [ HH.text $ "path:" <> printPath (state.path) ]
                ]
            , HH.img
                [ HP.class_ Rc.searchClear
                , HP.src searchIcon
                , HE.onClick (HE.input_ Clear)
                ]
            , HH.span
                [ HP.class_ B.inputGroupBtn ]
                [ HH.button
                    [ HP.classes [ B.btn, B.btnDefault ]
                    , HP.enabled (state.valid)
                    ]
                    [ glyph B.glyphiconSearch ]
                ]
            ]
        ]
    ]
  where
  searchClasses :: Array HH.ClassName
  searchClasses =
    [ B.inputGroup, Rc.searchInput] <> do
      guard (not $ state.valid)
      pure B.hasError

  value = these id id (\x y -> if x == "" then y else x) (state.value)
  searchIcon = if state.loading
               then "img/spin.gif"
               else "img/remove.svg"

eval :: Natural Query DSL
eval (Focus bool next) = do
  H.modify (_focused .~ bool)
  pure next
eval (Clear next) = do
  -- for peeking in parent
  pure next
eval (Typed str next) = do
  state <- H.get
  let c = state.timeout
  H.fromAff $ cancel c (error "timeout")
  c' <- H.fromAff $ forkAff
        $ later' Config.searchTimeout $ submit
        $ (state # _value %~ (thisOrBoth str <<< theseRight))
  H.modify $ _value %~ (thisOrBoth str <<< theseRight)
  H.modify $ _timeout .~ c'
  pure next
eval (Submit next) = do
  state <- H.get
  mbFn <- H.fromAff $ submit state
  case mbFn of
    Nothing -> pure unit
    Just fn -> H.modify fn
  pure next
eval (GetValue continue) = do
  state <- H.get
  pure $ continue $ theseRight (state.value)
eval (SetLoading bool next) = H.modify (_loading .~ bool) $> next
eval (SetValue tv next) = H.modify (_value .~ tv) $> next
eval (SetValid bool next) = H.modify (_valid .~ bool) $> next
eval (IsSearching continue) = do
  state <- H.get
  pure $ continue $ isJust $ theseRight $ state.value

submit :: State -> Aff SlamDataEffects (Maybe (State -> State))
submit state = do
  salt <- H.fromEff newSalt
  case theseLeft (state.value) of
    Just q -> case mkQuery q of
      Left _ | q /= "" -> pure $ pure (_valid .~ false)
      _ -> do
        H.fromEff $ setLocation
          $ browseURL (Just q) (state.sort) salt (state.path)
        pure Nothing
    _ -> pure Nothing
