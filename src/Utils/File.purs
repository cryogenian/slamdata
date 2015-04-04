module Utils.File (
  fileListToArray,
  files,
  newReader,
  readAsBinaryString,
  name,
  File(),
  FileList(),
  FileReader(),
  ReadFile()
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Exception

import Control.Monad.Aff
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
foreign import newReaderEff """
function newReaderEff() {
  return new FileReader();
}
""" :: forall e. Eff e FileReader

newReader :: forall e. Aff e FileReader
newReader = makeAff \_ k ->
  newReaderEff >>= \r -> k r
       

foreign import readAsBinaryStringEff """
function readAsBinaryStringEff(file) {
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


resultEff :: forall e. FileReader -> Eff (file::ReadFile|e) (Maybe String)
resultEff fr = runFn3 resultImpl Nothing Just fr

-- getting list of uploaded files has only `DOM` effect (we have not read it yet)
foreign import filesEff """
function filesEff(el) {
  return function() {
    if (!el.files) return [];
    return el.files;
  };
}
""" :: forall e. Node -> Eff (dom :: DOM|e) FileList

files :: forall e. Node -> Aff (dom :: DOM|e) FileList 
files node = makeAff \_ k -> do 
  fs <- filesEff node
  k fs 
  

foreign import onloadEff """
function onloadEff(reader) {
  return function(action) {
    return function() {
      reader.onload = function(ev) {
        action();
      };
    };
  };
}
""" :: forall e e'. FileReader -> Eff e Unit -> Eff (file :: ReadFile|e') Unit


readAsBinaryString :: forall e. File -> FileReader -> Aff (file :: ReadFile|e) String
readAsBinaryString file reader = makeAff \er k -> do 
  readAsBinaryStringEff file reader
  onloadEff reader $ do
    mbRes <- resultEff reader
    case mbRes of
      Nothing -> er $ error "files has not been read"
      Just res -> k $ res
  
