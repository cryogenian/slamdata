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

import SlamData.Prelude

import Data.Lens ((^.))

import Halogen.HTML.Core (HTML, ClassName)
import Halogen.HTML.Events as E
import Halogen.HTML as H
import Halogen.HTML.Properties as P
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Query (action)
import Halogen.Themes.Bootstrap3 as B

import SlamData.Hint as Hint
import SlamData.FileSystem.Component.CSS as CSS
import SlamData.FileSystem.Component.Query (Query(..))
import SlamData.FileSystem.Component.State (State, _showHiddenFiles, _isMount, _sort)
import SlamData.Common.Sort (Sort(..))

import Utils.DOM as DOM

sorting ∷ ∀ a. State → HTML a (Query Unit)
sorting state =
  H.div
    [ P.classes [ B.colXs4, CSS.toolbarSort ] ]
    [ H.a
      [ E.onClick \e → Just $ PreventDefault (DOM.toEvent e) $ action $ Resort ]
      [ H.text "Name"
      , H.i
        [ chevron (state ^. _sort)
        , ARIA.label $ label (state ^. _sort)
        , P.title $ label (state ^. _sort)
        ]
        []
      ]
    ]
  where
  chevron Asc = P.classes [ B.glyphicon, B.glyphiconChevronUp ]
  chevron Desc = P.classes [ B.glyphicon, B.glyphiconChevronDown ]
  label Asc = "Sort files by name descending"
  label Desc = "Sort files by name ascending"

toolbar ∷ ∀ p. State → HTML p (Query Unit)
toolbar state =
  H.div
    [ P.classes [ B.colXs5, CSS.toolbarMenu ] ]
    $ ( guard state.presentMountHint $>
        Hint.render
          Hint.RightArrow
          (H.ClassName "sd-mount-guide")
          DismissMountHint
          "To begin exploring data, please press the Mount button"
      )
    <> [ H.ul
         [ P.classes [ B.listInline, B.pullRight ] ]
         $ configure <> [ showHide, download, mount, folder, file, workspace ]
       ]
  where
  configure ∷ Array (HTML p (Query Unit))
  configure = do
    guard $ state ^. _isMount
    pure $ toolItem Configure "Configure mount" B.glyphiconWrench

  showHide ∷ HTML p (Query Unit)
  showHide =
    if state ^. _showHiddenFiles
    then toolItem HideHiddenFiles "Hide hidden files" B.glyphiconEyeClose
    else toolItem ShowHiddenFiles "Show hidden files" B.glyphiconEyeOpen

  download ∷ HTML p (Query Unit)
  download = toolItem Download "Download" B.glyphiconCloudDownload

  mount ∷ HTML p (Query Unit)
  mount = toolItem MakeMount "Mount database" B.glyphiconHdd

  folder ∷ HTML p (Query Unit)
  folder = toolItem MakeFolder "Create folder" B.glyphiconFolderClose

  file ∷ HTML p (Query Unit)
  file =
    H.li_
      [ H.input
          [ P.type_ P.InputFile
          , E.onChange (map (action <<< FileListChanged) <<< DOM.fromNode <<< DOM.target)
          , P.id_ "upload"
          ]
      , H.label
          [ P.for "upload"
          , P.title "Upload file"
          , ARIA.label "Upload file"
          ]
          [ H.i
              [ P.classes [ B.glyphicon, B.glyphiconCloudUpload, CSS.hiddenFileInput ] ]
              []
          ]
      ]

  workspace ∷ HTML p (Query Unit)
  workspace = toolItem MakeWorkspace "Create workspace" B.glyphiconBook

toolItem ∷ ∀ p f. (Unit → f Unit) → String → ClassName → HTML p (f Unit)
toolItem func title icon =
  H.li_
    [ H.button
        [ ARIA.label title
        , P.title title
        , E.onClick (E.input_ func)
        ]
        [ H.i
            [ P.title title
            , P.classes [ B.glyphicon, icon ]
            ]
            []
        ]
    ]
