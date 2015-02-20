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


-- | Link that will be passed to search query in router
type Link = {href :: String, name :: String}

-- | Output signals: init breadcrumbs, set breadcrumbs links to
-- | signal content
type Output = {links :: [Link]}

emptyOut :: Output
emptyOut = {links: []}

-- | Input signal: go to search location specified in link
data Input = Init | GoTo Link

renderLink :: Receiver Input _ -> Link -> VTree
renderLink send link =
  li {} [a {"href": jsVoid, "onclick": send $ GoTo link} [vtext link.name]]

-- | Renders breadcrumb and send <code>GoTo</code> message
-- | if clicked
render :: Receiver Input _ -> Output -> Eff _ VTree
render send out = do
  return $ ol {"className": "breadcrumb"} (
    (li {} [i {"className": "glyphicon glyphicon-chevron-right"} []]) :
    (renderLink send <$> out.links)
    )

-- | Transforming input signal to output signal 
run :: Input -> Output -> Eff _ Output
run input output =
  case input of
    Init -> return output
    GoTo link -> do
      log link
      return output
