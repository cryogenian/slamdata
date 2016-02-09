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

module SlamData.FileSystem.Component.Render where

import Prelude

import Control.MonadPlus (guard)

import Data.Functor (($>))
import Data.Lens ((^.))

import Halogen.HTML.Core (HTML(), ClassName())
import Halogen.HTML.CSS.Indexed as CSS
import Halogen.HTML.Events.Handler as E
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Query (action)
import Halogen.Themes.Bootstrap3 as B

import CSS.Geometry (marginLeft)
import CSS.Size

import SlamData.FileSystem.Component.Query
import SlamData.FileSystem.Component.State
import SlamData.FileSystem.Listing.Sort (Sort(..))
import SlamData.Render.CSS as Rc

sorting :: forall a. State -> HTML a (Query Unit)
sorting state =
  H.div [ P.classes [ B.colXs4, Rc.toolbarSort ] ]
  [ H.a [ E.onClick (\_ -> E.preventDefault $> action Resort) ]
    [ H.text "Name"
    , H.i [ chevron (state ^. _sort)
          , CSS.style (marginLeft $ px 10.0)
          ] [ ]
    ]
  ]
  where
  chevron Asc = P.classes [ B.glyphicon, B.glyphiconChevronUp ]
  chevron Desc = P.classes [ B.glyphicon, B.glyphiconChevronDown ]

toolbar :: forall p. State -> HTML p (Query Unit)
toolbar state =
  H.div [ P.classes [ B.colXs5, Rc.toolbarMenu ] ]
  [ H.ul [ P.classes [ B.listInline, B.pullRight ] ]
    $ configure <> [ showHide, download, mount, sqlMount, folder, file, notebook ]
  ]
  where
  configure :: Array (HTML p (Query Unit))
  configure = do
    guard $ state ^. _isMount
    pure $ toolItem
      Configure
      "Configure mount"
      B.glyphiconWrench

  showHide :: HTML p (Query Unit)
  showHide =
    if state ^. _showHiddenFiles
    then toolItem HideHiddenFiles "Hide hidden files" B.glyphiconEyeClose
    else toolItem ShowHiddenFiles "Show hidden files" B.glyphiconEyeOpen

  download :: HTML p (Query Unit)
  download = toolItem Download "Download" B.glyphiconCloudDownload

  mount :: HTML p (Query Unit)
  mount = toolItem MakeMount "Mount database" B.glyphiconHdd

  folder :: HTML p (Query Unit)
  folder = toolItem MakeFolder "Create folder" B.glyphiconFolderClose

  file :: HTML p (Query Unit)
  file =
    H.li_ [ H.button [ E.onClick (\ev -> pure $ action (UploadFile ev.target))]
            [ H.i [ P.title "Upload file"
                  , ARIA.label "Upload file"
                  , P.classes [ B.glyphicon
                              , B.glyphiconCloudUpload
                              , Rc.hiddenFileInput
                              ]
                  ]
              [ H.input [ P.inputType P.InputFile
                        , E.onChange (\ev -> pure $ action (FileListChanged ev.target))
                        ]
              ]
            ]
          ]

  notebook :: HTML p (Query Unit)
  notebook = toolItem MakeNotebook "Create notebook" B.glyphiconBook

  sqlMount :: HTML p (Query Unit)
  sqlMount = toolItem MakeSQLView "Mount sql view" B.glyphiconOpenFile


toolItem :: forall p f. (Unit -> f Unit)
            -> String -> ClassName -> HTML p (f Unit)
toolItem func title icon =
  H.li_ [ H.button [ ARIA.label title
                   , P.title title
                   , E.onClick (\_ -> pure $ func unit)
                   ]
          [ H.i [ P.title title
                , P.classes [ B.glyphicon, icon ]
                ]
            [ ]
          ]
        ]
