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

module Utils.File where

import SlamData.Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Data.Foreign (readString)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import DOM.File.FileReader as FR
import DOM.File.Types (File, fileReaderToEventTarget, fileToBlob)
import DOM.HTML.Event.EventTypes as Events

readAsText ∷ ∀ e. File → Aff (dom ∷ DOM | e) String
readAsText file = makeAff \er k → do
  reader ← liftEff FR.fileReader
  let
    et = fileReaderToEventTarget reader
    listener = eventListener \_ → do
      removeEventListener Events.load listener false et
      hush ∘ runExcept ∘ readString <$> FR.result reader >>=
        case _ of
          Nothing → er $ error "files has not been read"
          Just res → k res
  addEventListener Events.load listener false et
  FR.readAsText (fileToBlob file) reader
