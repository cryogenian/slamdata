module View.File.Search where

import Data.Inject1 (inj)
import Controller.File.Search (handleSearchSubmit, handleSearchChange, handleSearchClear)
import Data.Monoid (mempty)
import Input.File (FileInput(Focus))
import Model.File (State())
import View.Common (glyph)
import View.File.Common (I())
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as Vc

search :: forall e. State -> H.HTML (I e)
search state =
  H.div [ A.classes [B.colXs12, B.colSm8, Vc.search] ]
        [ H.form [ A.class_ B.navbarForm
                 , E.onSubmit (\_ -> pure $ handleSearchSubmit state.search state.path)
                 ]
                 [ H.div [ A.classes ([B.inputGroup, Vc.searchInput] <> if state.search.valid then mempty else [B.hasError])]
                         [ H.input [ A.classes [B.formControl]
                                   , A.value state.search.value
                                   , A.title state.search.value
                                   , E.onFocus (E.input_ $ inj $ Focus true)
                                   , E.onBlur (E.input_ $ inj $ Focus false)
                                   , E.onInput (\v -> pure $ handleSearchChange state.search v state.path)
                                   ]
                                   []
                         , H.span [ A.class_ (if state.search.focused then Vc.searchPathActive else Vc.searchPath) ]
                                  [ H.span [ A.class_ Vc.searchPathBody ]
                                           [ H.text state.search.nextValue ]
                                  , H.span [ A.class_ (if state.search.nextValue == "" then Vc.searchAffixEmpty else Vc.searchAffix) ]
                                           [ H.text $ "path:" <> state.path ]
                                  ]
                         , H.img [ E.onClick (\_ -> pure $ handleSearchClear (state.searching && state.search.loading) state.search)
                                 , A.class_ Vc.searchClear
                                 , (if state.search.loading then A.src "img/spin.svg" else A.src "img/remove.svg")
                                 ]
                                 []
                         , H.span [ A.class_ B.inputGroupBtn ]
                                  [ H.button [ A.classes [B.btn, B.btnDefault]
                                             , A.disabled (not state.search.valid)
                                             ]
                                             [ glyph B.glyphiconSearch ]
                                  ]
                         ]
                 ]
        ]
