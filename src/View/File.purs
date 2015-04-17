module View.File (view) where

import Data.Either
import Data.Tuple
import Data.Maybe
import Data.Monoid (mempty)
import Data.Array
import Control.Functor
import Control.Apply
import Utils.Halide 
import qualified Model.File as M
import qualified Model.Item as M
import qualified Model.Sort as Ms
import qualified Model.Resource as Mr
import qualified Model.Path as M
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Forms as F
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Data.StrMap as SM
import qualified Config as Config
import qualified View.Css as Vc

homeHash :: String
homeHash = "#?sort=asc&q=path%3A%2F&salt="

icon :: forall a i node. (H.HTMLRepr node) => node a i
icon =
  H.div [A.classes [B.colXs1, Vc.navIcon]] [
    H.a [A.href homeHash,
         A.classes [B.navbarBrand, Vc.logo]] [
       H.i [A.classes [B.glyphicon, B.glyphiconFolderOpen]][]
       ]
    ]

logo :: forall a i node. (H.HTMLRepr node) => node a i
logo =
  
  H.div [A.classes [B.colXs3, Vc.navLogo]] [
    H.a [A.href Config.slamDataHome, A.classes [B.navbarBrand, Vc.logo]] [
      H.img [A.src "./logo.svg"] []
      ]
    ]
 
search :: forall u node. (H.HTMLRepr node) => M.State ->
          node u (Either M.Input M.Request)
search state =
  H.div [A.classes [B.colXs12, B.colSm8, Vc.search]] [
    H.form [A.class_ B.navbarForm,
            E.onsubmit (\_ -> pure $ Right $ M.SearchSubmit state.search state.path)] [
       H.div [A.classes ([B.inputGroup, Vc.searchInput] <> if state.search.valid then
                                              mempty
                                            else [B.hasError])] [
          H.input [A.classes [B.formControl],
                   A.value state.search.value,
                   A.title state.search.value,
                   E.onfocus (\_ -> pure <<< Left $ M.Focus true),
                   E.onblur (\_ -> pure <<< Left $ M.Focus false),
                   F.onInput (\v ->
                               pure <<< Right $ M.SearchChange
                               state.search v state.path)
                   ] [],
          H.span [A.class_ (if state.search.focused then
                              Vc.searchPathActive
                              else
                              Vc.searchPath)]
          [H.span [A.class_ Vc.searchPathBody]
            [H.text  state.search.nextValue],
           H.span [A.class_ (if state.search.nextValue == ""
                             then Vc.searchAffixEmpty
                             else Vc.searchAffix)] [
             H.text $ "path:" <> state.path]],

          
                              
          H.img [E.onclick
                 (\_ -> pure <<< Right $ M.SearchClear (state.searching && state.search.loading) state.search),
                 A.class_ Vc.searchClear,
                 (if state.search.loading
                  then A.src "./spin.svg"
                  else A.src "./remove.svg")][],


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
    (breadcrumbView <$> state.breadcrumbs)
  where breadcrumbView b =
          H.li_ [H.a (targetLink <<< M.Breadcrumb $ b)
                 [H.text b.name]] 

sorting :: forall u node. (H.HTMLRepr node) =>
           M.State -> node u (Either M.Input M.Request)
sorting state =
    H.div [A.classes [B.colXs4, Vc.toolbarSort]] [
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
  H.div [A.classes [B.colXs8, Vc.toolbarMenu]] [
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
        Boolean -> Number -> M.Item -> node u (Either M.Input M.Request)
item searching ix state =
  H.div [A.classes ([B.listGroupItem] <> if state.selected then
                                          [B.listGroupItemInfo]
                                          else mempty),
         E.onmouseover (const <<< back $ M.ItemHover ix true),
         E.onclick (const <<< back $ M.ItemSelect ix true),
         E.ondblclick (const <<< request $ M.Open state)] [
    H.div [A.class_ B.row] [
       H.div [A.classes [B.colSm9, Vc.itemContent]] [
         H.a (targetLink <<< M.Open $ state) [
            H.span [] [
               H.i [iconClasses state] [],
               H.text $ (if searching
                         then M.decodeURIPath state.root
                         else "") <> state.name
               ]
            ]
         ],
       H.div [A.classes [B.colSm3, Vc.itemToolbar]] [
         H.ul [A.classes ([B.listInline, B.pullRight] <>
                         if not $ state.hovered || state.selected then
                           [B.hidden]
                         else mempty),
               A.style $ A.styles $ SM.fromList [Tuple "margin-bottom" "0"]]
         (showToolbar state)
         ]
       ]
    ]
  where iconClasses state = A.classes [B.glyphicon,
                                       Vc.itemIcon,
                                       iconClass state.resource]
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
              H.li_ [H.a
                     (targetLink <<< M.Move $ item)
                     [H.i [A.classes [B.glyphicon, B.glyphiconMove],
                           A.title "move/rename"] []]],
              H.li_ [H.a (targetLink <<< M.Delete $ item)
                     [H.i [A.classes [B.glyphicon, B.glyphiconTrash],
                           A.title "remove"][]]],
              H.li_ [H.a
                     (targetLink <<< M.Share $ item)
                     [H.i [A.classes [B.glyphicon, B.glyphiconShare],
                           A.title "share"][]]]
              ]
            

items :: forall u node. (H.HTMLRepr node) => M.State ->
         node u (Either M.Input M.Request)
items state =
  H.div [A.classes [B.listGroup, Vc.results]] $
  ((uncurry (item state.searching)) <$>  zip (0..length state.items) state.items)

modal :: forall a node. (H.HTMLRepr node) =>
                 M.State -> 
                 node a (Either M.Input M.Request)
modal state =
  H.div [E.onclick (\ev -> pure (Left $ M.SetDialog Nothing)),
         A.classes 
         ([B.modal, B.fade] <> maybe [] (const [B.in_]) state.dialog)] [
    
    H.div [A.classes [B.modalDialog]] [
       H.div [E.onclick (\_ -> E.stopPropagation $> Left M.Resort),
              A.classes [B.modalContent]] 
       (modalContent state)
       ]
    ]

    where h4 str = [H.h4_ [H.text str]]
          header children =
            H.div [A.classes [B.modalHeader]] children
          body children =
            H.div [A.classes [B.modalBody]] children
          footer children =
            H.div [A.classes [B.modalFooter]] children
          modalContent state =
            case state.dialog of
              Just (M.ShareDialog url) ->
                [header $ h4 "URL",
                 body [
                   H.form_ [
                      H.div [
                         A.classes [B.formGroup],
                         E.onclick (\ev -> pure (Right $ M.ToSelect ev.target))] [
                         H.input [
                            readonly true,
                            A.classes [B.formControl],
                            A.value url][]
                         ]
                      ]
                   ],
                      
                 footer [
                   H.button [
                      A.id_ "copy-button",
                      E.onclick (\_ -> pure (Right $ M.ToClipboard url)),
                      A.classes [B.btn, B.btnPrimary]] [
                      H.text "Copy"
                      ]
                   ]]
              Just M.MountDialog -> 
                [header $ h4 "Mount",
                 body [], footer []]
              Just M.RenameDialog ->
                [header $ h4 "Rename",
                 body [], footer[]]
              Just M.ConfigureDialog ->
                [header $ h4 "Configure",
                 body [], footer []]
              Nothing -> []
                

    
                         

    

view :: forall u node. (H.HTMLRepr node) => M.State ->
        node u (Either M.Input M.Request)
view state =
  H.div_ [
    navbar [
       
       H.div [A.classes [Vc.navCont, B.containerFluid]][
          icon,
          logo,
          search state
          ]
       ],
    content [] [
      breadcrumbs state,
      row [
        sorting state,
        toolbar state
        ],
      items state
      ],
    modal state
    ]

  where content :: forall a i node. (H.HTMLRepr node) =>
                   [A.Attr i] -> [node a i] -> node a i
        content attrs nodes =  H.div ([A.class_ B.container] <> attrs) [
          H.div [A.class_ B.row] [
             H.div [A.classes [B.colMd8, B.colMdOffset2,
                               B.colSm10, B.colSmOffset1]] nodes
             ]
          ]

        row :: forall a i node. (H.HTMLRepr node) => [node a i] -> node a i
        row = H.div [A.class_ B.row]

        navbar :: forall a i node. (H.HTMLRepr node) => [node a i] ->  node a i
        navbar = H.nav [A.classes [B.navbar, B.navbarInverse, B.navbarFixedTop]]

        
