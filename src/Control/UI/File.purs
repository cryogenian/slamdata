{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Control.UI.File
       ( fileListToArray
       , files
       , newReader
       , newReaderEff
       , readAsBinaryString
       , readAsBinaryStringEff
       , name
       , File
       , FileList
       , FileReader
       , READ_FILE
       ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Aff (Aff, makeAff)

import Data.Function (Fn3, runFn3)
import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.HTML.Types (HTMLElement)


foreign import data FileReader :: *
foreign import data File :: *
foreign import data FileList :: *
foreign import data READ_FILE :: !

foreign import fileListToArray :: FileList -> Array File
foreign import name :: forall e. File -> Eff (file :: READ_FILE |e) String
foreign import newReaderEff :: forall e. Eff (dom :: DOM |e) FileReader
foreign import readAsBinaryStringEff :: forall e. File -> FileReader ->
                                        Eff (file :: READ_FILE |e) Unit
foreign import resultImpl :: forall e a. Fn3 (Maybe a) (a -> Maybe a)
                             FileReader (Eff (file :: READ_FILE |e) (Maybe String))
foreign import filesEff :: forall e. HTMLElement -> Eff (dom :: DOM |e) FileList
foreign import onloadEff :: forall e e'. FileReader -> Eff e Unit ->
                            Eff (file :: READ_FILE |e') Unit

newReader :: forall e. Aff (dom :: DOM |e) FileReader
newReader = makeAff \_ k ->
  newReaderEff >>= \r -> k r

resultEff :: forall e. FileReader -> Eff (file :: READ_FILE |e) (Maybe String)
resultEff fr = runFn3 resultImpl Nothing Just fr

files :: forall e. HTMLElement -> Aff (dom :: DOM |e) FileList
files node = makeAff \_ k -> do
  fs <- filesEff node
  k fs

readAsBinaryString :: forall e. File -> FileReader -> Aff (file :: READ_FILE|e) String
readAsBinaryString file reader = makeAff \er k -> do
  readAsBinaryStringEff file reader
  onloadEff reader do
    mbRes <- resultEff reader
    case mbRes of
      Nothing -> er $ error "files has not been read"
      Just res -> k res
