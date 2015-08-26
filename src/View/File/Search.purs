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

module View.File.Search where

import Prelude
import Data.Inject1 (inj)
import Controller.File.Search (handleSearchSubmit, handleSearchChange, handleSearchClear)
import Data.Monoid (mempty)
import Data.These (these)
import Input.File (FileInput(..))
import Input.File (FileInput(..))
import Model.File
import Model.File.Search
import View.Common (glyph)
import View.File.Common (HTML())
import Data.Path.Pathy
import Optic.Core
import Optic.Refractor.Prism (_Just)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as Vc

search :: forall e. State -> HTML e
search state =
  let ss = state ^. _search
      value = these id id (\x y -> if x == "" then y else x) (ss ^. _value)
  in
    H.div [ A.classes [Vc.search] ]
          [ H.form [ E.onSubmit (\_ -> pure $ handleSearchSubmit state) ]
                   [ H.div [ A.classes ([B.inputGroup, Vc.searchInput] <>
                                        if ss ^. _valid
                                        then mempty
                                        else [B.hasError])
                           ]
                           [ H.input [ A.classes [B.formControl]
                                     , A.value value
                                     , E.onFocus (E.input_ $ inj $ WithState (_search .. _focused .~ true))
                                     , E.onBlur (E.input_ $ inj $ WithState (_search .. _focused .~ false))
                                     , E.onInput (pure <<< handleSearchChange state)
                                     ]
                                     []
                           , H.span [ A.class_ (if ss ^. _focused
                                                then Vc.searchPathActive
                                                else Vc.searchPath)
                                    ]
                                    [ H.span [ A.class_ Vc.searchPathBody ]
                                             [ H.text value ]
                                    , H.span [ A.class_ (if value == ""
                                                         then Vc.searchAffixEmpty
                                                         else Vc.searchAffix) ]
                                             [ H.text $ "path:" <> printPath (state ^. _path) ]
                                    ]
                           , H.img [ E.onClick (\_ -> pure $ handleSearchClear state)
                                   , A.class_ Vc.searchClear
                                   , A.src $ searchIcon (ss ^. _loading)
                                   ]
                                   []
                           , H.span [ A.class_ B.inputGroupBtn ]
                                    [ H.button [ A.classes [B.btn, B.btnDefault]
                                               , A.disabled (not (ss ^. _valid))
                                               ]
                                               [ glyph B.glyphiconSearch ]
                                    ]
                           ]
                   ]
          ]
  where

  searchIcon :: Boolean -> String
  searchIcon true = "img/spin.gif"
  searchIcon false = "img/remove.svg"
