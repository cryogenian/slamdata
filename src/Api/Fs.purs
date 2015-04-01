-- | This module will be reworked after `affjax` is ready.
module Api.Fs (
  meta, Metadata(..),
  metadata,
  makeNotebook,
  deleteItem,
  makeFile,
  move
  ) where

import Control.Monad.Eff
import DOM (DOM())
import Data.Foreign
import Data.Foreign.Class
import Data.Maybe
import Data.Foldable
import Data.Traversable 
import Data.Either
import Data.Function

import qualified Network.HTTP.Affjax.Response as Af
import qualified Network.HTTP.Affjax.ResponseType as At
import qualified Network.HTTP.Affjax as Af
import qualified Data.DOM.Simple.Ajax as A
import qualified Data.Array as Ar
import qualified Data.String as Str
import qualified Model as M

newtype Metadata = Metadata {
  children :: [M.Item]
  }

instance metadataResponse :: Af.Responsable Metadata where
  responseType _ = At.JSONResponse
  fromResponse a = pure $ Metadata {children: []}

meta :: forall e. String -> Af.Affjax e Metadata
meta path = Af.get path



-- gets raw metadata
metadataF :: forall e. String -> (Foreign -> Eff (dom::DOM|e) Unit) -> 
            Eff (dom::DOM|e) Unit 
metadataF path callback = do
  req <- A.makeXMLHttpRequest
  let action = do
        state <- A.readyState req
        case state of
          A.Done -> do
            response <- A.responseText req
            status <- A.status req
            case parseJSON response of
              Right f | status == 200 -> callback f
              _ -> pure unit 
          _ -> pure unit
          
  A.onReadyStateChange action req
  A.open A.GET (Config.metadataUrl <> path) req
  A.send A.NoData req

-- | gets current directory children
metadata :: forall e. String -> ([M.Item] -> Eff (dom :: DOM | e) Unit) ->
            Eff (dom :: DOM|e) Unit 
metadata path callback = 
  metadataF path $ \f -> do
    let readed = readProp "children" f >>= readArray >>= traverse M.readItem
    either (const $ pure unit) callback readed 
          
          
foreign import f2jsonImpl """
function f2jsonImpl(f, nothing, just ) {
  var res = JSON.stringify(f);
  if (typeof res == 'undefined') {
    return nothing;
  } else if (typeof f == 'string') {
    return just(f);
  }
  else {
    return just(res)
  }
}
""" :: forall a. Fn3 Foreign (Maybe a) (a -> Maybe a) (Maybe String)

-- converting js-objects to json representation
-- if input is json-string will return it
f2json :: Foreign -> Maybe String
f2json f = runFn3 f2jsonImpl f Nothing Just 

-- | put raw data
putF :: forall e. String -> Foreign -> (Boolean -> Eff (dom::DOM|e) Unit) -> 
       Eff (dom::DOM|e) Unit 
putF path obj callback = do 
  req <- A.makeXMLHttpRequest
  let action = do
        state <- A.readyState req
        case state of
          A.Done -> do
            status <- A.status req
            callback $ status == 200
          _ -> return unit
  case f2json obj of
    Nothing -> pure unit
    Just obj -> do 
      A.onReadyStateChange action req
      A.open A.PUT (Config.dataUrl <> path) req
      A.send (A.JsonData obj) req


makeNotebook :: forall e. String -> M.Notebook ->
                (Boolean -> Eff (dom::DOM|e) Unit) ->
                Eff (dom::DOM|e) Unit 
makeNotebook path notebook callback = 
  putF path (toForeign notebook) callback


makeFile :: forall e. String -> String ->
            (Boolean -> Eff (dom :: DOM|e) Unit) -> 
            Eff (dom :: DOM|e) Unit 
makeFile path content callback = 
  let isJson = either (const false) (const true) $ do
        hd <- maybe (Left (JSONError "incorrect")) Right $
              Ar.head $ Str.split "\n" content
        parseJSON hd
  in if isJson then do
    putF path (toForeign content) callback
     else
     callback false


move :: forall e. String -> String -> (Boolean -> Eff (dom::DOM|e) Unit) ->
        Eff (dom::DOM|e) Unit
move src tgt callback = do 
  req <- A.makeXMLHttpRequest
  
  let action = do
        state <- A.readyState req
        case state of 
          A.Done -> do
            status <- A.status req
            callback $ status == 200
          _ -> return unit
          
  A.onReadyStateChange action req
  A.open (A.HttpMethod "MOVE") (Config.dataUrl <> src) req
  A.setRequestHeader "Destination" tgt req
  A.send A.NoData req 

-- | delete item by path
delete :: forall e. String -> (Boolean -> Eff (dom :: DOM | e) Unit) ->
           Eff (dom::DOM|e) Unit 
delete path callback = do
  req <- A.makeXMLHttpRequest
  
  let action = do
        state <- A.readyState req
        case state of 
          A.Done -> do
            status <- A.status req
            callback $ status == 200
          _ -> return unit
          
  A.onReadyStateChange action req
  A.open (A.HttpMethod "DELETE") (Config.dataUrl <> path) req
  A.send A.NoData req 

-- | delete item
deleteItem :: forall e. M.Item -> (Boolean -> Eff (dom :: DOM|e) Unit) ->
              Eff (dom :: DOM|e) Unit 
deleteItem item callback = 
  let path = item.root <> item.name <>
      if item.resource /= M.File && item.resource /= M.Notebook then "/"
        else ""
  in delete path callback
                                                   
