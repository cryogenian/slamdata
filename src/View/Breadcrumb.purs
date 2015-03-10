-- | Breadcrumb component is used for navigation up and bottom
-- | in directory structure and to show where is user now
-- | In this module I won't use terms <code>Action</code>
-- | and <code>State</code>.
module View.Breadcrumb where

import Control.Monad.Eff
import DOM (DOM())
import View.Shortcuts (a, jsVoid, ol, li, i)
import VirtualDOM.VTree (VTree(), vtext)
import VirtualDOM.Events (hook)
import Component (Receiver())
import Data.String (split)
import Data.Array (filter, reverse)
import Data.Foldable (foldl)
import Signal.Channel (Chan())

import qualified Hash as Hash
import qualified Router as Router


type Link = {link :: String, name :: String}

type Output = {links :: [Link]}

emptyOut :: Output
emptyOut = {links: []}

data Input = Init | Update [Link]

goto :: forall e. Link -> Eff (dom::DOM|e) Unit
goto {link: link} = do
  Router.setPath link

renderLink :: forall e. Receiver Input e -> Link -> VTree
renderLink send link =
  li {} [a {"href": jsVoid, "click": hook "click" $ const (goto link)}
         [vtext link.name]]

render :: forall e. Receiver Input (dom::DOM, chan::Chan|e) -> 
          Output -> Eff (dom::DOM, chan::Chan|e) VTree
render send out = do
  return $ ol {"className": "breadcrumb"} (
    (li {} [i {"className": "glyphicon glyphicon-chevron-right"} []]) :
    (renderLink send <$> out.links)
    )

run :: forall e. Input -> Output -> Eff e Output
run input output =
  case input of
    Init -> return output
    Update links -> do
      return {links: links}

hookFn :: forall e. Receiver Input (chan::Chan|e) -> Eff (chan::Chan|e) Unit
hookFn receiver =
  Hash.changed $ do
    path <- Router.extractPath <$> Router.getRoute
    let parts = filter (\x -> x /= "") $ split "/" path
        links = reverse $ foldl (\(head:tail) a->
                        let res = {name: a, link: head.link <> a <> "/"} in
                        res:head:tail) [{name: "root", link: "/"}] parts
    receiver $ Update links
    
