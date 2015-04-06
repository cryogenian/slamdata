module View (view) where

import Data.Either
import Data.Tuple
import Data.Monoid (mempty)
import Data.Array
import Utils.Halide (back, request, targetLink)
import qualified Model as M
import qualified Model.Item as M
import qualified Model.Sort as Ms
import qualified Model.Resource as Mr
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Forms as F
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Data.StrMap as SM
import qualified Config as Config
import qualified View.Css as Vc

logo :: forall a i node. (H.HTMLRepr node) => node a i
logo =
  H.div [A.class_ B.colSm3] [
    H.a [A.class_ B.navbarBrand, A.href "#?sort=asc&q="] [
       H.i [A.classes [B.glyphicon, B.glyphiconFolderOpen]] []
       ],
    H.a [A.href Config.slamDataHome, A.class_ B.navbarBrand] [
      H.text "SlamData"
      ]
    ]

search :: forall u node. (H.HTMLRepr node) => M.State ->
          node u (Either M.Input M.Request)
search state =
  H.div [A.class_ B.colSm7] [
    H.form [A.class_ B.navbarForm,
            E.onsubmit (\_ -> pure $ Right $ M.SearchSubmit state.search)] [
       H.div [A.classes ([B.inputGroup, Vc.searchInput] <> if state.search.valid then
                                              mempty
                                            else [B.hasError])] [
          H.input [A.classes [B.formControl],
                   A.value state.search.value,
                   (F.onInput (pure <<< Right <<< M.SearchChange state.search.timeout))
                   ] [],
          H.span [A.class_ B.inputGroupBtn] [
            H.button [A.classes [B.btn, B.btnDefault],
                      A.disabled (not state.search.valid)] [
               H.i [A.classes [B.glyphicon, B.glyphiconSearch]] []
               ]
            ]
          ]
       ]
    ]

breadcrumbs :: forall u node. (H.HTMLRepr node) =>
               M.State -> node u (Either M.Input M.Request)
breadcrumbs state =
  H.ol [A.class_ B.breadcrumb] $
    [H.li_ []] <>
    (breadcrumbView <$> state.breadcrumbs)
  where breadcrumbView b =
          H.li_ [H.a (targetLink <<< M.Breadcrumb $ b)
                 [H.text b.name]] 

sorting :: forall u node. (H.HTMLRepr node) =>
           M.State -> node u (Either M.Input M.Request)
sorting state =
    H.div [A.class_ B.colSm4] [
      H.a (targetLink <<< M.SetSort $ Ms.notSort state.sort) [
          H.text "Name",
          H.i [chevron state,
               A.style (A.styles $ SM.fromList [Tuple "margin-left" "10px"])] []
          ]
       ]
    where chevron {sort: Ms.Asc} = A.classes [B.glyphicon, B.glyphiconChevronUp]
          chevron {sort: Ms.Desc} = A.classes [B.glyphicon, B.glyphiconChevronDown]

toolbar :: forall u node. (H.HTMLRepr node) =>
           M.State -> node u (Either M.Input M.Request)
toolbar state =
  H.div [A.class_ B.colSm8] [
    H.ul [A.classes [B.listInline, B.pullRight]]
    if inRoot state then [mount]
    else  [
      file,
      folder, 
      mount,
      notebook
      ]
    ]
  where mount = H.li_ [H.a (targetLink <<< M.MountDatabase $ state)
                       [H.i [A.title "mount database",
                             A.classes [B.btnLg, B.glyphicon, B.glyphiconHdd]][]]]
        file = H.li_
               [H.a 
                [A.href "javascript:void(0);",
                 E.onclick (\ev -> request $ M.UploadFile ev.target state)][
                  H.i [A.classes [B.btnLg, B.glyphicon, B.glyphiconFile],
                       A.title "upload file"]
                  [H.input [A.class_ B.hidden,
                            A.type_ "file",
                            E.onchange (
                              \ev -> request $
                                     M.FileListChanged ev.target state)][]]]]
        folder = H.li_ [H.a (targetLink <<< M.CreateFolder $ state)
                        [H.i [A.title "create folder",
                              A.classes [B.btnLg,
                                         B.glyphicon,
                                         B.glyphiconFolderClose]][]]]
        notebook = H.li_ [H.a (targetLink <<< M.CreateNotebook $ state)
                          [H.i [A.title "create notebook",
                                A.classes [B.btnLg,
                                           B.glyphicon,
                                           B.glyphiconBook]][]]]
        inRoot state = state.path == "" || state.path == "/"


item :: forall u node. (H.HTMLRepr node) =>
        Number -> M.Item -> node u (Either M.Input M.Request)
item ix state =
  H.div [A.classes ([B.listGroupItem] <> if state.selected then
                                          [B.listGroupItemInfo]
                                          else mempty),
         E.onmouseover (const <<< back $ M.ItemHover ix true),
         E.onclick (const <<< back $ M.ItemSelect ix true),
         E.ondblclick (const <<< request $ M.Open state)] [
    H.div [A.class_ B.row] [
       H.div [A.class_ B.colSm6] [
          H.a (targetLink <<< M.Open $ state) [
             H.i [iconClasses state] [],
             H.span [A.style $ A.styles $ SM.fromList [Tuple "margin-left" "20px"]] [
               H.text state.name
               ]
             ]
          ],
       H.div [A.class_ B.colSm6] [
         H.ul [A.classes ([B.listInline, B.pullRight] <>
                         if not $ state.hovered || state.selected then
                           [B.hidden]
                         else mempty),
               A.style $ A.styles $ SM.fromList [Tuple "margin-bottom" "0"]]
         (showToolbar state)
         ]
       ]
    ]
  where iconClasses state = A.classes [B.glyphicon, iconClass state.resource]
        iconClass res = case res of
          Mr.File -> B.glyphiconFile
          Mr.Database -> B.glyphiconHdd
          Mr.Notebook -> B.glyphiconBook
          Mr.Directory -> B.glyphiconFolderOpen
          Mr.Table -> B.glyphiconTh

        showToolbar :: forall node. (H.HTMLRepr node) =>
                      M.Item -> [node u (Either M.Input M.Request)]
        showToolbar item =
          if item.name == M.up then []
            else
            let conf = case item.resource of
                  Mr.Database ->
                    [H.li_
                     [H.a (targetLink <<< M.Configure $ item)
                      [H.i [A.classes [B.glyphicon, B.glyphiconWrench],
                            A.title "configure"] []]]]
                  _ -> []
            in conf <> [
              H.li_ [H.a (targetLink <<< M.Move $ item)
                     [H.i [A.classes [B.glyphicon, B.glyphiconMove],
                           A.title "move/rename"] []]],
              H.li_ [H.a (targetLink <<< M.Delete $ item)
                     [H.i [A.classes [B.glyphicon, B.glyphiconTrash],
                           A.title "remove"][]]],
              H.li_ [H.a (targetLink <<< M.Share $ item)
                     [H.i [A.classes [B.glyphicon, B.glyphiconShare],
                           A.title "share"][]]]
              ]
            

items :: forall u node. (H.HTMLRepr node) => M.State ->
         node u (Either M.Input M.Request)
items state =
  H.div [A.classes [B.listGroup, Vc.results]] $ zipWith item (0..length state.items) state.items



view :: forall u node. (H.HTMLRepr node) => M.State ->
        node u (Either M.Input M.Request)
view state =
  H.div_ [
    navbar [
       content [
          logo,
          search state
          ]
       ],
    content [
      breadcrumbs state,
      row [
        sorting state,
        toolbar state
        ],
      items state
      ]
    ]
  where content :: forall a i node. (H.HTMLRepr node) => [node a i] -> node a i
        content =  H.div [A.class_ B.container]

        row :: forall a i node. (H.HTMLRepr node) => [node a i] -> node a i
        row = H.div [A.class_ B.row]

        navbar :: forall a i node. (H.HTMLRepr node) => [node a i] ->  node a i
        navbar = H.nav [A.classes [B.navbar, B.navbarInverse, B.navbarFixedTop]]
