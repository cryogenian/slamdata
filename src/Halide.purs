module Halide where

import Data.Either
import Halogen
import Halogen.Signal
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import Halogen.Mixin.UndoRedo
import Halogen.HTML.Events
import Halogen.HTML.Events.Handler
import Halogen.HTML.Events.Types
import qualified Halogen.Themes.Bootstrap3 as B

import qualified Halide.Logo as Logo

import qualified Config as Config
data Input = Inc | Dec

data Request = Up

back :: forall a e. Input -> Event (|e) -> EventHandler (Either Input Request)
back i _ = pure $ Left i

request :: forall a e. Request -> Event (|e) -> EventHandler (Either Input Request)
request r _ = pure $ Right r 

data State = State Number

ui :: forall u. SF1 Input (H.HTML u (Either Input Request))
ui = view <$> stateful (State 0) updateState

navbar :: A.ClassName
navbar = A.className "navbar"

navbarInverse :: A.ClassName
navbarInverse = A.className "navbar-inverse"

navbarFixedTop :: A.ClassName
navbarFixedTop = A.className "navbar-fixed-top"

navbarA :: forall i. H.Attribute i
navbarA = A.classes [navbar, navbarInverse, navbarFixedTop]

glyphicon :: A.ClassName
glyphicon = A.className "glyphicon"

glyphiconChevronRight :: A.ClassName
glyphiconChevronRight = A.className "glyphicon-chevron-right"

btnLink :: A.ClassName
btnLink = A.className "btn-link"

style :: forall i. String -> H.Attribute i
style = A.attribute $ H.attributeName "style"

listInline :: A.ClassName
listInline = A.className "list-inline"

pullRight :: A.ClassName
pullRight = A.className "pull-right"

listGroup :: A.ClassName
listGroup = A.className "list-group"

listGroupItem :: A.ClassName
listGroupItem = A.className "list-group-item"

navbarForm :: A.ClassName
navbarForm = A.className "navbar-form"

nav :: A.ClassName
nav = A.className "nav"

navbarNav :: A.ClassName
navbarNav = A.className "navbar-nav"

navbarRight :: A.ClassName
navbarRight = A.className "navbar-right"

navbarBrand :: A.ClassName
navbarBrand = A.className "navbar-brand"

view :: forall u. State -> H.HTML u (Either Input Request)
view _ =
  H.div_ [
    H.nav navbarA [
       H.div (A.class_ B.container) [
          H.div (A.class_ B.colSmall3) [
             H.a (A.href "javascrip:void(0);" <> A.class_ navbarBrand) [
                H.i (A.classes [glyphicon, glyphiconChevronRight]) []
                ],
             H.a (A.href Config.slamDataHome <> A.class_ navbarBrand) [
               H.text "SlamData"
               ]
             ],
          H.div (A.class_ B.colSmall7) [
            H.form (A.class_ navbarForm) [
               H.div (A.class_ B.inputGroup) [
                  H.span (A.class_ B.inputGroupAddon) [H.text "Path:"],
                  H.input (A.class_ B.formControl) [],
                  H.span (A.class_ B.inputGroupBtn) [
                    H.button (A.classes [B.btn, B.btnDefault]) [
                       H.i (A.classes [glyphicon, glyphiconChevronRight]) []
                       ]
                    ]
                  ]
               ]
            ],
          H.div (A.class_ B.colSmall2) [
            H.ul (A.classes [nav, navbarNav, navbarRight]) [
               H.li_ [H.a (A.href "javascript:void(0);") [H.text "Foo"]]
               ]
            ]
          ]
       ],
    H.div (A.class_ B.container) [
      H.ol (A.class_ B.breadcrumb) [
         H.li_ [H.i (A.classes [glyphicon, glyphiconChevronRight]) []]
         ],
      H.div (A.class_ B.row) [
        H.div (A.class_ B.colSmall4) [
           H.button (A.classes [B.btn, btnLink]) [
              H.text "Name",
              H.i (A.classes [glyphicon, glyphiconChevronRight] <>
                   style "margin-left:10px") []
              ]
           ],
        H.div (A.class_ B.colSmall8) [
          H.ul (A.classes [listInline, pullRight]) [
             H.li_ [H.button (A.classes [B.btnSmall, btnLink]) [H.text "File"]],
             H.li_ [H.button (A.classes [B.btnSmall, btnLink]) [H.text "Folder"]],
             H.li_ [H.button (A.classes [B.btnSmall, btnLink]) [H.text "Mount"]],
             H.li_ [H.button (A.classes [B.btnSmall, btnLink]) [H.text "Notebook"]]
             ]
          ]
        ],
      H.div (A.class_ listGroup) [
        H.div (A.class_ listGroupItem) [
           H.div (A.class_ B.row) [
              H.div (A.class_ B.colSmall6) [
                 H.button (A.classes [B.btn, btnLink]) [
                    H.i (A.classes [glyphicon, glyphiconChevronRight]) [],
                    H.span (style "margin-left:20px") [H.text "trololo"]
                    ]
                 ],
              H.div (A.class_ B.colSmall6) [
                H.ul (A.classes [listInline, pullRight] <>
                      style "margin-bottom:0") [
                   H.li_ [H.button (A.classes [B.btn, btnLink]) [H.text "link"]]
                   ]
                ]
              ]
           ]
        ]
      ]
    ]


updateState :: State -> Input -> State
updateState (State count) Inc = State (count + 1)
updateState (State count) Dec = State (count - 1)


