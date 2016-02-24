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

module SlamData.Notebook.StyleLoader where

import Prelude

import Control.UI.Browser (decodeURIComponent)

import Data.Array as Arr
import Data.Maybe as M
import Data.Either as E
import Data.Foldable as F
import Data.Nullable as N
import Data.String as Str
import Data.String.Regex as Rgx
import Data.Traversable as T
import Data.URI as URI
import Data.URI.Types as URI

import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Location as Location
import DOM.HTML.Window as Window
import DOM.HTML.Types (htmlDocumentToDocument, htmlDocumentToParentNode, htmlDocumentToNode)
import DOM.Node.Types (Node(), elementToNode)
import DOM.Node.Document (createElement)
import DOM.Node.Element as Element
import DOM.Node.Node as Node
import DOM.Node.ParentNode as ParentNode

import Control.Monad.Eff (Eff())

retrieveStyles
  :: forall e
   . Eff (dom :: DOM|e) (Array URI.URIRef)
retrieveStyles =
  window
    >>= Window.location
    >>= Location.search
    <#> additionalStyles
    <#> map URI.runParseURIRef
    <#> F.foldMap (E.either (\_ -> []) pure)
  where
  additionalStyles :: String -> Array String
  additionalStyles s =
    M.fromMaybe []
      $ Str.split ","
      <$> extractStyleURIStr s

  extractStyleURIStr :: String -> M.Maybe String
  extractStyleURIStr str =
    Rgx.match stylesRgx str
      >>= flip Arr.index 1
      >>= id
      <#> decodeURIComponent

  stylesRgx :: Rgx.Regex
  stylesRgx =
    Rgx.regex "cssStyleSheets=([^&]+)" Rgx.noFlags


createLink
  :: forall e
   . URI.URIRef
  -> Eff (dom :: DOM|e) Node
createLink uriRef = do
  doc <- window >>= Window.document
  link <- createElement "link" $ htmlDocumentToDocument doc
  Element.setAttribute "type" "text/css" link
  Element.setAttribute "rel" "stylesheet" link
  Element.setAttribute "href" (URI.printURIRef uriRef) link
  pure $ elementToNode link


getHead
  :: forall e
   . Eff (dom :: DOM|e) Node
getHead = do
  doc <- window >>= Window.document
  nullableHead <-
    ParentNode.querySelector "head"
    $ htmlDocumentToParentNode doc
  case map elementToNode $ N.toMaybe nullableHead of
    M.Just head -> pure head
    M.Nothing -> do
      newHead <-
        map elementToNode
        $ createElement "head"
        $ htmlDocumentToDocument doc
      Node.appendChild newHead $ htmlDocumentToNode doc
      pure newHead

loadStyles :: forall e. Eff (dom :: DOM|e) Unit
loadStyles =
  retrieveStyles
    >>= T.traverse createLink
    >>= F.traverse_ \link -> do
      h <- getHead
      Node.appendChild link h
