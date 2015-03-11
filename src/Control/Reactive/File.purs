-- | Uploading file module probably will be moved out to separate project
module Control.Reactive.File (
  FileReader(..), File(..), FileList(..),
  fileListToArray, name, file2blob, newFormData,
  append2FormData, newReader, readAsBinaryString,
  str2blob, onload, result, files, uploader
  ) where

import VirtualDOM.VTree (VTree(), VHook(), vnode)
import Utils (log, parent)
import VirtualDOM.Events (hook, composeHooks)
import Control.Apply
import Control.Monad.Eff
import Control.Reactive.Event (Event(), target, raiseEvent)
import Data.DOM.Simple.Element (querySelector)
import Data.DOM.Simple.Types (HTMLElement())
import Data.DOM.Simple.Ajax (Blob(), FormData())
import DOM (DOM())
import Data.Maybe
import Data.Array (head)
import Data.Function


-- | prependFileUploader "li" {} [vtext "foo"] = 
-- |  vnode "li" {} [input {type: "file"}]
foreign import data FileReader :: *
foreign import data File :: *
foreign import data FileList :: *

foreign import fileListToArray """
function fileListToArray(fl) {
  var result = [];
  for (var i = 0; i < fl.length; i++) {
    result.push(fl[i]);
  }
  return result;
}
""" :: FileList -> [File]

foreign import name """
function name(file) {
  return function() {
    return file.name;
  };
}
""" :: forall e. File -> Eff e String

foreign import file2blob """
function file2blob(file) {return file;}
""" :: File -> Blob


foreign import newFormData """
function newFormData() {
  return new FormData();
}
""" :: forall e. Eff e FormData

foreign import append2FormData """
function append2FormData(name) {
  return function(a) {
    return function(fd) {
      return function() {
        fd.append(name, a);
        return fd;
      };
    };
  };
}
""" :: forall e a. String -> a -> FormData -> Eff e FormData 

foreign import newReader """
function newReader() {
  return new FileReader();
}
""" :: forall e. Eff e FileReader

foreign import readAsBinaryString """
function readAsBinaryString(file) {
  return function(reader) {
    return function() {
      reader.readAsBinaryString(file);
    };
  };
}
""" :: forall e. File -> FileReader -> Eff e Unit

foreign import str2blob """
function str2blob(str) {
  return new Blob([str]);
}
""" :: forall e. String -> Blob 

foreign import onload """
function onload(action) {
  return function(reader) {
    return function() {
      reader.onload = function(ev) {
        action(ev)();
      };
      return reader;
    };
  };
}
""" :: forall e e'. (Event -> Eff e Unit) -> FileReader -> Eff e' FileReader

foreign import resultImpl """
function resultImpl(nothing, just, fr) {
  return function() {
    var res = fr.result;
    if (res === null) return nothing;
    return just(res);
  };
}
""" :: forall e a. Fn3 (Maybe a) (a -> Maybe a) FileReader (Eff e (Maybe String))

result :: forall e. FileReader -> Eff e (Maybe String)
result fr = runFn3 resultImpl Nothing Just fr


foreign import files """
function files(el) {
  return function() {
    if (!el.files) return [];
    return el.files;
  };
}
""" :: forall e. HTMLElement -> Eff e FileList


-- Internal 
proxy :: forall e. Event -> Eff (dom::DOM|e) Unit
proxy evt = do
  mbInput <- target evt >>= querySelector "input"
  case mbInput of
    Nothing -> return unit
    Just input -> void $ raiseEvent "click" input {}

-- Internal
act :: forall e. Event -> Eff (dom::DOM|e) Unit
act event = do
  el <- target event
  fileList <- files el
  case head <<< fileListToArray $ fileList of
    Nothing -> return unit
    Just file -> void $ do
      par <- parent el
      void $ raiseEvent "filechanged" el {file: file}
      reader <- newReader
      readAsBinaryString file reader
      (flip onload) reader $ \evt -> void $ do
        cont <- result reader
        case cont of
          Nothing -> return unit
          Just res -> void $  raiseEvent "fileparsed" el {content: res, file: file}



uploader :: forall p a. String -> {click::VHook|p} -> [VTree]  -> VTree
uploader name props children = 
  vnode name props{"click" = hook "click" proxy `composeHooks` props.click} $ 
  (vnode "input" {"type": "file",
                  "change": hook "change" act,
                  "style": {"display": "none"}} []):children
  
  
