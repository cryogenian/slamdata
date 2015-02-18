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
type Action = String

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
foldState st state = return st


construct :: forall e. Eff (chan::Chan|e) (Service Action State _)
construct = do
  current <- getHash
  chan <- channel current
  onHashChange $ do
    getHash >>= send chan
  signal <- foldpE foldState current (subscribe chan)


  return $  {
    signal: signal,
    send: send chan
    }



    
