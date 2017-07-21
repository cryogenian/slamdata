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

module SlamData.FileSystem.Dialog.Mount.Component.State where

import SlamData.Prelude

import Data.List.NonEmpty as NEL
import Data.Map as M
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.String as String
import Quasar.Mount as QM
import Quasar.Types (AnyPath)
import SlamData.Dialog.Component as Dialog
import SlamData.FileSystem.Dialog.Mount.MountAction as MA
import SlamData.FileSystem.Dialog.Mount.Scheme as MS
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN
import Utils.Path (DirPath)

type ErrorReply = String → Slam Unit

data ErrorType = NameError | InnerError | OuterError

derive instance eqErrorType ∷ Eq ErrorType
derive instance ordErrorType ∷ Ord ErrorType

type State =
  { input ∷ Input
  , name ∷ Maybe String
  , scheme ∷ Maybe MS.Scheme
  , errors ∷ M.Map ErrorType String
  , value ∷ Maybe QM.MountConfig
  , errorReply ∷ ErrorReply
  }

data Input
  = New { parent ∷ DirPath }
  | Edit { parent ∷ Maybe DirPath, name ∷ Maybe String, mount ∷ QM.MountConfig }
  | Root

scheme ∷ QM.MountConfig → MS.Scheme
scheme = case _ of
  QM.MongoDBConfig _ → MS.MongoDB
  QM.ViewConfig _ → MS.SQL2
  QM.CouchbaseConfig _ → MS.Couchbase
  QM.MarkLogicConfig _ → MS.MarkLogic
  QM.SparkFTPConfig _ → MS.SparkFTP
  QM.SparkHDFSConfig _ → MS.SparkHDFS
  QM.SparkLocalConfig _ → MS.SparkLocal
  QM.ModuleConfig _ → MS.SQL2

matchSchemeConfig ∷ MS.Scheme → Input → Maybe QM.MountConfig
matchSchemeConfig s = case _ of
  Edit { mount } | s == scheme mount → Just mount
  _ → Nothing

initialState ∷ Input → State
initialState input =
  { input
  , name: toName input
  , scheme: scheme <$> originalMount input
  , errors: M.empty
  , value: toValue input
  , errorReply: const (pure unit)
  }
  where
    toName ∷ Input → Maybe String
    toName = case _ of
      New _ → Just ""
      Edit { name } → name
      Root → Nothing
    toValue ∷ Input → Maybe QM.MountConfig
    toValue = case _ of
      Edit { mount } → Just mount
      _ → Nothing

parent ∷ Input → Maybe DirPath
parent = case _ of
  New { parent: p } → Just p
  Edit { parent: p } → p
  Root → Nothing

originalMount ∷ Input → Maybe QM.MountConfig
originalMount = case _ of
  Edit { mount } → Just mount
  _ → Nothing

isNew ∷ Input → Boolean
isNew = case _ of
  New _ → true
  _ → false

validateName ∷ String → Maybe String
validateName name = either Just (const Nothing) do
  when (String.trim name == "") $ Left "Please enter a mount name"
  when (String.contains (String.Pattern "/") name) $ Left "Mount names cannot contain slashes"
  pure name

pathName ∷ State → Maybe (Either P.DirName P.FileName)
pathName st =
  case st.scheme of
    Just MS.SQL2 → Right ∘ P.FileName <$> st.name
    Just _ → Left ∘ P.DirName <$> st.name
    Nothing → Nothing

path ∷ State → Maybe AnyPath
path st = do
  pn ← pathName st
  case st.input of
    Root → Just (Left P.rootDir)
    _ → do
      pp ← parent st.input
      pure $ bimap ((pp </> _) ∘ P.dir') ((pp </> _) ∘ P.file') pn

-- TODO: save button label
buttonsFromState ∷ State → Dialog.Buttons MA.MountAction
buttonsFromState st =
  let
    p = path st
  in
    appendUnmountButton p $
      Dialog.buttonDefault "Cancel" Dialog.Dismiss
        `NEL.cons` pure (saveButton p "Save")
  where
    saveButton ∷ Maybe AnyPath → String → Dialog.Button MA.MountAction
    saveButton p label =
      Dialog.Button
        { label
        , class_: CN.btnPrimary
        , action: maybe Dialog.Dismiss Dialog.Confirm $
            MA.SaveMount <$> p <*> st.value <*> pure st.errorReply
        , disabled: isNothing st.value
        }
    appendUnmountButton
      ∷ Maybe AnyPath
      → Dialog.Buttons MA.MountAction
      → Dialog.Buttons MA.MountAction
    appendUnmountButton p =
      case originalMount st.input of
        Just m | isNothing (parent st.input) →
          NEL.cons $
            Dialog.Button
              { label: "Unmount"
              , action: maybe Dialog.Dismiss Dialog.Confirm $ MA.DeleteMount <$> p
              , class_: CN.btnDanger
              , disabled: false
              }
        _ → id
