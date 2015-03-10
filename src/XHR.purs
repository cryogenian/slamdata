module XHR where

import Control.Monad.Eff
import Signal hiding (map)
import Utils (log)

import DOM (DOM())
import Data.Foldable (for_)
import Data.StrMap (StrMap(), toList)
import Data.Tuple (Tuple(..))
import qualified Data.DOM.Simple.Ajax as A

import Component (Receiver())

type Input a = {
  method :: A.HttpMethod,
  content :: A.HttpData a,
  additionalHeaders :: StrMap String,
  url :: String
  }

type Output = {
  content :: String
  }

run :: forall a e. Receiver Output (dom::DOM|e) ->  Input a ->  Eff (dom::DOM|e) Unit
run sendToOutput inp = do 
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

