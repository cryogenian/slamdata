-- | Low level url-hash service

module Hash  where

import Control.Monad.Eff
import Signal
import Signal.Channel
import Signal.Effectful
import View.Shortcuts
import Data.String.Regex
import Utils
import Component
import Control.Monad.Eff.Ref


type Hash = String
type State = String
type Action = String

foreign import getHashImpl """
function getHashImpl() {
  return document.location.hash;
}
""" :: forall e. Eff e Hash

getHash = do
  let rgx = regex "^#" noFlags
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

initialState :: Eff _ State
initialState = getHash


-- | If we have a message to set hash we just set hash
foldState :: Action -> State -> Eff _ State
foldState st state = return st

-- | constructing service
construct :: forall e. Eff (chan::Chan|e) (Service Action State (chan::Chan|e))
construct = do
  -- we get hash
  current <- getHash
  -- to send it as initial state
  chan <- channel current
  -- listen hashchange
  onHashChange $ do
    getHash >>= send chan
  -- this signal will just emit hash-string without "#" 
  signal <- foldpE foldState current (subscribe chan)

  return {
    signal: signal,
    send: send chan
    }




    
