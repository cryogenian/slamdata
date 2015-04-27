module View.Notebook (view, HTML()) where

import Data.Maybe (maybe)
import Control.Functor (($>))
import Data.Inject1 (inj)
import Control.Plus (empty)
import View.Common (contentFluid, navbar, icon, logo, glyph)
import Data.Array ((..), length, zipWith, replicate)
import Model.Notebook (Input(..), State(..))
import Model.Notebook.Menu (DropdownItem(), MenuElement())
import Controller.Notebook (handleMenuSignal)
import Data.Int (toNumber, fromNumber, Int())
import Data.String (joinWith)
import EffectTypes (NotebookAppEff())

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Config as Config
import qualified View.Css as Vc

type HTML p e = H.HTML p (E.Event (NotebookAppEff e) Input)

view :: forall p e. State -> HTML p e
view state =
  H.div [ E.onClick (E.input_ CloseDropdowns) ]
  (navigation state <> body state)


navigation :: forall p e. State -> [HTML p e]
navigation state =
  if not state.editable
  then []
  else
    [ navbar
      [ H.div [ A.classes [ Vc.navCont, Vc.notebookNav, B.containerFluid ] ]
        [ icon B.glyphiconBook
        , logo
        , name state ]
      , H.ul [ A.classes [ B.nav, B.navbarNav ] ]
        ( zipWith li (0 .. length state.dropdowns) state.dropdowns )
      ] ]

body :: forall p e. State -> [HTML p e]
body state =
  [ if not state.loaded
    then H.h1 [ A.classes [ B.textCenter ] ] [H.text "Loading..." ]
    else if state.error /= ""
         then H.div [ A.classes [ B.alert, B.alertDanger ] ]
              [ H.h1 [ A.classes [ B.textCenter ] ] [H.text state.error] ]
         else contentFluid [ H.div [ A.class_ B.clearfix ]  [ ] ] ]

txt :: forall p e. Int -> String -> [HTML p e]
txt lvl text =
  [ H.text $ (joinWith "" $ replicate (toNumber lvl) "--") <> " " <> text ]


li :: forall p e. Number ->  DropdownItem -> HTML p e
li i {visible: visible, name: name, children: children} =
  H.li [ E.onClick (\ev -> do E.stopPropagation
                              E.input_ (Dropdown i) ev)
       , A.classes $ [ B.dropdown ] <>
         (if visible then [ B.open ] else [ ]) ]
  [ H.a [ A.href "#"
        , E.onClick (\_ -> E.preventDefault $> empty)] (txt (fromNumber 0) name)
  , H.ul [ A.classes [ B.dropdownMenu ] ]
    (menuItem <$> children) ]

menuItem :: forall p e. MenuElement -> HTML p e
menuItem {name: name, message: mbMessage, lvl: lvl} =
  H.li [ A.classes (maybe [B.disabled] (const []) mbMessage) ]
  [ H.a [ A.href "#"
        , E.onClick (\e -> E.preventDefault $>
                           maybe empty (handleMenuSignal <<< inj) mbMessage) ]
    [H.span_ $ (txt lvl name) <>
     (maybe [glyph B.glyphiconChevronRight] (const []) mbMessage) ]]


name :: forall p e. State -> HTML p e
name state =
  H.div [ A.classes [ B.colXs12, B.colSm8 ] ]
  [ H.input [ A.classes [ Vc.notebookName ]
            , E.onInput (E.input SetName)
            , A.value (state.name)  ] [] ]

