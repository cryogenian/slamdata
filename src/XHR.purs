-- | Low level service for managing xhr calls
-- | Probably will be removed after moving to purescript-aff
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

type Input a = {
  method :: A.HttpMethod,
  content :: A.HttpData a,
  additionalHeaders :: StrMap String,
  url :: String
  }

type Output = {
  content :: String
  }

run :: forall a. Receiver Output _ ->  Input a ->  Eff _ Unit
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

