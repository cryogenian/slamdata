{-
Copyright 2015 SlamData, Inc.

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

module FileSystem.Effects where

import DOM (DOM())
import Control.Monad.Eff.Random (RANDOM())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff.Ref (REF())
import Network.HTTP.Affjax (AJAX())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Console (CONSOLE())
import Control.UI.ZClipboard (ZCLIPBOARD())
import Control.UI.File (READ_FILE())
import Data.Date (Now())

type FileSystemEffects = ( dom :: DOM
                         , random :: RANDOM
                         , avar :: AVAR
                         , ajax :: AJAX
                         , ref :: REF
                         , console :: CONSOLE
                         , zClipboard :: ZCLIPBOARD
                         , err :: EXCEPTION
                         , file :: READ_FILE
                         , now :: Now
                         )

type FileSystemRawEffects = ( random :: RANDOM
                            , ajax :: AJAX
                            , ref :: REF
                            , console :: CONSOLE
                            , zClipboard :: ZCLIPBOARD
                            , file :: READ_FILE
                            , now :: Now
                            )
