module Utils.File where

import Control.Monad.Eff
import DOM
import Data.Function
import Data.Maybe
import Data.DOM.Simple.Types (HTMLElement())
import Data.DOM.Simple.Ajax (Blob(), FormData())

foreign import data FileReader :: *
foreign import data File :: *
foreign import data FileList :: *
foreign import data ReadFile :: !

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
""" :: forall e. File -> Eff (file :: ReadFile|e) String

-- Creating new FileReader doesn't produce any effects 
-- since it can be not used in example and than garbage collected in example. 
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
""" :: forall e. File -> FileReader -> Eff (file :: ReadFile|e) Unit


foreign import resultImpl """
function resultImpl(nothing, just, fr) {
  return function() {
    var res = fr.result;
    if (res === null) return nothing;
    return just(res);
  };
}
""" :: forall e a.
       Fn3 (Maybe a)
       (a -> Maybe a)
       FileReader
       (Eff (file :: ReadFile|e) (Maybe String))


result :: forall e. FileReader -> Eff (file::ReadFile|e) (Maybe String)
result fr = runFn3 resultImpl Nothing Just fr


-- getting list of uploaded files has only `DOM` effect (we have not read it yet)
foreign import files """
function files(el) {
  return function() {
    if (!el.files) return [];
    return el.files;
  };
}
""" :: forall e. HTMLElement -> Eff (dom :: DOM|e) FileList

foreign import onload """
function onload(reader) {
  return function(action) {
    return function() {
      reader.onload = function(ev) {
        action();
      };
      return reader;
    };
  };
}
""" :: forall e e'. FileReader -> Eff e Unit -> Eff (file :: ReadFile|e') FileReader


