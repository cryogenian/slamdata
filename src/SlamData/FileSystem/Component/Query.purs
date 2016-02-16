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

module SlamData.FileSystem.Component.Query where

import DOM.HTML.Types (HTMLElement())

import SlamData.FileSystem.Listing.Sort (Sort())
import SlamData.FileSystem.Routing.Salt (Salt())

import Utils.Path (DirPath())

data Query a
  = Resort a
  | SetPath DirPath a
  | SetSort Sort a
  | SetSalt Salt a
  | SetIsMount Boolean a
  | Configure a
  | ShowHiddenFiles a
  | HideHiddenFiles a
  | Download a
  | MakeMount a
  | MakeFolder a
  | MakeNotebook a
  | UploadFile HTMLElement a
  | FileListChanged HTMLElement a
  | SetVersion String a
  | MakeSQLView a
  | DismissSignInSubmenu a
