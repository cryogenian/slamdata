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

module Model.Item where

import Prelude

import Control.Monad.Eff (Eff())
import Control.UI.Browser (setLocation)

import Data.Maybe (Maybe(..))
import Data.Path.Pathy (printPath)

import DOM (DOM())

import Model.AccessType (AccessType(..), printAccessType)
import Model.Common (browseURL)
import Model.Resource (Resource(..), resourcePath, resourceName, sortResource)
import Model.Salt (Salt())
import Model.Sort (Sort())
import Utils.Path (encodeURIPath)

data Item
  = Item Resource
  | SelectedItem Resource
  | PhantomItem Resource

itemResource :: Item -> Resource
itemResource (Item r) = r
itemResource (SelectedItem r) = r
itemResource (PhantomItem r) = r

itemURL :: Sort -> Salt -> AccessType -> Item -> String
itemURL sort salt act item = case itemResource item of
  File path ->
    Config.notebookUrl ++ "#/explore" ++ encodeURIPath (printPath path)
  ViewMount path ->
    Config.notebookUrl ++ "#/explore" ++ encodeURIPath (printPath path)
  Notebook path ->
    Config.notebookUrl ++ "#" ++ encodeURIPath (printPath path) ++ printAccessType act
  Directory path ->
    browseURL Nothing sort salt path
  Database path ->
    browseURL Nothing sort salt path


openItem :: forall e. Item -> Sort -> Salt -> Eff (dom :: DOM|e) Unit
openItem (PhantomItem _) _ _ = pure unit
openItem item sort salt =
  setLocation $ itemURL sort salt Editable item


sortItem :: Boolean -> Sort -> Item -> Item -> Ordering
sortItem isSearching sort a b =
  sortResource (sortProjection isSearching) sort (itemResource a) (itemResource b)
  where
  sortProjection true = resourcePath
  sortProjection _ = resourceName

instance eqItem :: Eq Item where
  eq (Item r) (Item r') = r == r'
  eq (SelectedItem r) (SelectedItem r') = r == r'
  eq (PhantomItem r) (PhantomItem r') = r == r'
  eq _ _ = false

instance ordItem :: Ord Item where
  compare (SelectedItem r) (SelectedItem r') = compare r r'
  compare (SelectedItem _) _ = GT
  compare _ (SelectedItem _) = LT
  compare (PhantomItem r) (PhantomItem r') = compare r r'
  compare (PhantomItem _) _ = LT
  compare _ (PhantomItem _) = GT
  compare (Item r) (Item r') = compare r r'
