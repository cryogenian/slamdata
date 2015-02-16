module Hash  where

import Control.Monad.Eff
import Signal
import Signal.Channel
import Signal.Effectful
import View.Shortcuts
import Data.String.Regex
import Utils
import Component


type Hash = String
type State = String
data Action = SearchQuery String 

foreign import getHashImpl """
function getHashImpl() {
  return document.location.hash;
}
""" :: forall e. Eff e Hash

getHash = do
  let rgx = regex "^#" {
        unicode: false,
        sticky: false,
        global: false,
        multiline: false,
        ignoreCase: false
        }
  raw <- getHashImpl
  return $ replace rgx "" raw
  

foreign import setHashImpl """
function setHashImpl(string) {
  return function() {
    document.location.hash = string;
  };
}
""" :: forall e. String -> Eff e Unit

setHash :: forall e. String -> Eff e Unit
setHash hash = do
  setHashImpl hash



foreign import onHashChange """
function onHashChange(action) {
  return function() {
    window.addEventListener('hashchange', function() {
      action();
    });
  };
}
""" :: forall e a. Eff e Unit -> Eff e Unit


matcher :: Regex
matcher = regex ".+" {
  global: true,
  ignoreCase: false,
  multiline: false,
  sticky: false,
  unicode: false
  }


foldState :: Action -> State -> Eff _ State
foldState (SearchQuery st) state = return st


foldAll :: Action -> Folder State -> Eff _ (Folder State)
foldAll action {state: state, current: current, previous: previous} = do
  new <- foldState action state
  setHash new
  return $ mkFolder new

construct :: forall e. Eff (chan::Chan|e) (Component Action State)
construct = do
  current <- getHash
  chan <- channel (SearchQuery current)
  onHashChange $ do
    getHash >>= \x -> send chan (SearchQuery x)
  signal <- foldpE
            foldAll
            (mkFolder current)
            (subscribe chan)

  return {
    signal: signal,
    channel: chan,
    vt: emptyVTree
    }


    
