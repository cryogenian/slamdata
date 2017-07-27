{-
Copyright 2017 SlamData, Inc.

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

module SlamData.FileSystem.Dialog.Rename.Component (dialog) where

import SlamData.Prelude

import DOM.Event.Event as DEE
import Data.Array as Array
import Data.Maybe.First (First(..))
import Data.Path.Pathy (printPath, rootDir)
import Data.Variant as V
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import SlamData.Dialog.Component as D
import SlamData.Dialog.Render as DR
import SlamData.FileSystem.Dialog.Rename.Component.State as S
import SlamData.FileSystem.Listing.Item.Component.CSS as ItemCSS
import SlamData.FileSystem.Resource as R
import SlamData.GlobalError as GE
import SlamData.Monad (Slam)
import SlamData.Quasar.FS as API
import SlamData.Render.ClassName as CN
import SlamData.Render.Common (formGroup)
import Utils.DOM as DOM
import Utils.Path (DirPath, dropWorkspaceExt)

data Query a
  = Init a
  | ToggleShowList Boolean a
  | NameTyped String a
  | DirTyped String a
  | DirClicked R.Resource a
  | StopPropagation DOM.Event (Query a)
  | Dismiss a
  | Confirm R.Resource a

type Message o = Variant (renamed ∷ Unit | o)
type DSL o = H.ComponentDSL S.State Query (D.Message (Message o)) Slam
type HTML = H.ComponentHTML Query

dialog ∷ ∀ o. R.Resource → D.DialogSpec (Message o) Slam
dialog res =
  D.dialog
    $ D.withTitle ("Rename “" <> dropWorkspaceExt (R.resourceName res) <> "”")
    >>> D.withInitialState (S.initialState res)
    >>> D.withClass (H.ClassName "sd-rename-dialog")
    >>> D.withRender render
    >>> D.withInitializer Init
    >>> D.withEval eval
    >>> D.withPending _.renaming
    >>> D.withButton
        (D.button
          $ D.withLabel "Cancel"
          >>> D.withAction (const (Just Dismiss)))
    >>> D.withButton
        (D.button
          $ D.withLabel "Rename"
          >>> D.withClass CN.btnPrimary
          >>> D.withAction (either (const Nothing) (Just ∘ Confirm) ∘ _.value)
          >>> D.withPending _.renaming)

render ∷ S.State → HTML
render state =
  HH.div
    [ HP.class_ (H.ClassName "sd-rename-dialog-inner")
    , HE.onClick \e → Just $ StopPropagation (DOM.toEvent e) $ H.action $ ToggleShowList false
    ]
    $ join
        [ pure (renderName state.name)
        , pure dirDropdownField
        , pure dirDropdownList
        , foldMap (pure ∘ DR.renderError) $
            ala First foldMap [either Just (const Nothing) state.value, state.error]
        ]

  where

  dirDropdownField ∷ HTML
  dirDropdownField =
    HH.div
      [ HP.classes [ CN.inputGroup, HH.ClassName "file-list-field" ] ]
      [ HH.input
          [ HP.type_ HP.InputText
          , HP.classes [ CN.formControl ]
          , HP.placeholder "New directory"
          , HE.onValueInput (HE.input DirTyped)
          , HP.value state.dir
          ]
      , HH.span
          [ HP.classes [ CN.inputGroupBtn ] ]
          [ HH.button
              [ HP.type_ HP.ButtonButton
              , HP.classes [ CN.btn, CN.btnDefault ]
              , HE.onClick \e →
                   Just $ StopPropagation (DOM.toEvent e) $ H.action $ ToggleShowList (not state.showList)
              , ARIA.label "Select a destination folder"
              , HP.title "Select a destination folder"
              ]
              [ HH.span [ HP.classes [ CN.caret ] ] [ ] ]
          ]
      ]
  dirDropdownList ∷ HTML
  dirDropdownList =
    HH.ul
      ([ HP.classes  [ CN.listGroup, CN.fileListGroup ] ]
       <> if state.showList then [] else [ ARIA.hidden "true" ])
    $ renameItem <$> state.dirs

  renameItem ∷ R.Resource → HTML
  renameItem res =
    HH.button
      [ HP.classes ([ CN.listGroupItem ] <> (guard (R.isHidden res) $> ItemCSS.itemHidden))
      , HE.onClick (HE.input_ (DirClicked res))
      , HP.type_ HP.ButtonButton
      ]
      [ HH.text (R.resourcePath res) ]

renderName ∷ String → HTML
renderName name =
  formGroup
    [ HH.input
        [ HP.classes [ CN.formControl ]
        , HP.value name
        , HP.placeholder "New name"
        , HE.onValueInput (HE.input NameTyped)
        ]
    ]

eval ∷ ∀ o. Query ~> DSL o
eval = case _ of
  Init next → do
    state ← H.get
    dirItemClicked (R.parent state.initial)
    flip getDirectories rootDir \ds →
      H.modify \st → st { dirs = Array.sort (Array.nub (st.dirs <> ds)) }
    pure next
  StopPropagation e q → do
    H.liftEff (DEE.stopPropagation e)
    eval q
  ToggleShowList b next → do
    H.modify (_ { showList = b })
    pure next
  NameTyped name next → do
    modify (_ { name = name })
    pure next
  DirTyped dir next → do
    modify (_ { dir = dir })
    pure next
  DirClicked res next → do
    dirItemClicked res
    pure next
  Confirm dest next → do
    { initial } ← H.get
    H.modify (_ { error = Nothing, renaming = true })
    result ← H.lift $ API.move initial (R.getPath dest)
    case result of
      Left e →
        case GE.fromQError e of
          Left msg → H.modify (_ { error = Just msg, renaming = false })
          Right ge → GE.raiseGlobalError ge
      Right Nothing →
        H.modify (_ { error = Just "The file you are trying to move does not exist, please refresh.", renaming = false })
      Right (Just x) → do
        H.raise D.Dismiss
        H.raise (D.Bubble (V.inj (SProxy ∷ SProxy "renamed") unit))
    pure next
  Dismiss next → do
    H.raise D.Dismiss
    pure next

modify ∷ ∀ o. (S.State → S.State) → DSL o Unit
modify f = H.modify (S.validate ∘ f ∘ (_ { error = Nothing }))

dirItemClicked ∷ ∀ o. R.Resource → DSL o Unit
dirItemClicked res = do
  case R.getPath res of
    Right _ → pure unit
    Left dir → modify (_ { dir = printPath dir, showList = false })

getChildren
  ∷ ∀ o. (R.Resource → Boolean)
  → (Array R.Resource → DSL o Unit)
  → DirPath
  → DSL o Unit
getChildren pred cont start = do
  ei ← API.children start
  case ei of
    Right items → do
      let items' = Array.filter pred items
          parents = Array.mapMaybe (either Just (const Nothing) ∘ R.getPath) items
      cont items'
      traverse_ (getChildren pred cont) parents
    _ → pure unit

getDirectories ∷ ∀ o. (Array R.Resource → DSL o Unit) → DirPath → DSL o Unit
getDirectories = getChildren $ R.isDirectory ∨ R.isDatabaseMount
