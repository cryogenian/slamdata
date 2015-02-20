-- | Low level service for managing xhr calls
-- | It should be wrapped in Api 
module XHR where

import Control.Monad.Eff
import Signal hiding (map)
import Signal.Channel
import Signal.Effectful
import Utils

import DOM
import Data.Foldable (for_)
import Data.StrMap
import Data.Tuple (Tuple(..))
import qualified Data.DOM.Simple.Ajax as A

import Component

type SendRec a = {
  method :: A.HttpMethod,
  content :: A.HttpData a,
  additionalHeaders :: StrMap String,
  url :: String
  }

data Input a = Init | Send (SendRec a)

type Output = {
  content :: String
  }

emptyOutput = {
  content: ""
  }


run :: forall a. Receiver Output _ ->  Input a ->  Eff _ Unit
run sendToOutput input = 
  case input of
    Init -> return unit
    Send inp -> do 
      req <- A.makeXMLHttpRequest
      
      let action = do
            state <- A.readyState req
            case state of
              A.Done -> do
                content <- A.responseText req
                sendToOutput {content: content}
              _ -> return unit
              
      A.onReadyStateChange action req
      A.open inp.method inp.url req
      for_ (toList inp.additionalHeaders) (\(Tuple key value) -> do
                                                A.setRequestHeader key value req)
      A.send inp.content req



-- | oh... It will not send it anywhere, I need a global state or singleton
-- | or whatever
justSend :: forall a. SendRec a -> Eff _ Unit
justSend rc = run (const $ return unit) (Send rc)

construct :: forall e a. Eff (chan::Chan, dom::DOM|e) 
                         (Service (Input a) Output (chan::Chan,dom::DOM|e)) 
construct = do 
  input <- channel Init
  output <- channel emptyOutput
  runSignal $ subscribe input ~> \msg ->
    run (send output) msg
  return {
    signal: subscribe output,
    send: send input
    }
