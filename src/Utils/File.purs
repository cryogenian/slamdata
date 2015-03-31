module Utils.File where

import Control.Monad.Eff

import Data.Function
import Data.Maybe
import Data.DOM.Simple.Types (HTMLElement())
import Data.DOM.Simple.Ajax (Blob(), FormData())

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
""" :: forall e e'. FileReader -> Eff e Unit -> Eff e' FileReader

foreign import clearValue """
function clearValue(el) {
  return function() {
    el.value = null;
  };
}
""" :: forall e. HTMLElement -> Eff e Unit
