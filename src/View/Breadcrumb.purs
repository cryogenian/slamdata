-- | Breadcrumb component is used for navigation up and bottom
-- | in directory structure and to show where is user now
-- | In this module I won't use terms <code>Action</code>
-- | and <code>State</code>.
module View.Breadcrumb where

import DOM
import View.Shortcuts
import Utils
import Signal
import Signal.Channel
import Signal.Effectful
import VirtualDOM
import VirtualDOM.VTree
import Control.Monad.Eff
import VirtualDOM.Events
import Component
import Data.String
import Data.Array (filter, reverse)
import Data.Foldable
import qualified Hash as Hash
import qualified Router as Router

type Link = {link :: String, name :: String}

type Output = {links :: [Link]}

emptyOut :: Output
emptyOut = {links: []}

data Input = Init | Update [Link]

goto :: Link -> Eff _ Unit
goto {link: link} = do
  Router.setPath link

renderLink :: Receiver Input _ -> Link -> VTree
renderLink send link =
  li {} [a {"href": jsVoid, "click": hook "click" $ const (goto link)}
         [vtext link.name]]

render :: Receiver Input _ -> Output -> Eff _ VTree
render send out = do
  return $ ol {"className": "breadcrumb"} (
    (li {} [i {"className": "glyphicon glyphicon-chevron-right"} []]) :
    (renderLink send <$> out.links)
    )

run :: Input -> Output -> Eff _ Output
run input output =
  case input of
    Init -> return output
    Update links -> do
      return {links: links}

hookFn :: Receiver Input _ -> Eff _ Unit
hookFn receiver =
  Hash.changed $ do
    path <- Router.extractPath <$> Router.getRoute
    let parts = filter (\x -> x /= "") $ split "/" path
        links = reverse $ foldl (\(head:tail) a->
                        let res = {name: a, link: head.link <> a <> "/"} in
                        res:head:tail) [{name: "root", link: "/"}] parts
    receiver $ Update links
    
