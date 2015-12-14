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

module Dialog.Rename where

import Prelude

import Control.Monad (when)
import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff.Ref (REF())
import Control.Monad.Error.Class (throwError)
import Control.Monad.Free (Free())
import Control.UI.Browser (reload)
import Data.Array (elemIndex, singleton, sort, nub)
import Data.Date (Now())
import Data.Either (Either(..), either)
import Data.Functor (($>))
import Data.Lens ((^.), (%~), (.~), (?~), lens, LensP())
import Data.Maybe (Maybe(..), isNothing, isJust, maybe)
import Data.Monoid (mempty)
import Data.Path.Pathy (printPath, parseAbsDir, sandbox, rootDir, (</>))
import Data.String as S
import Data.Void (Void())
import Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import Halogen
import Halogen.CustomProps as Cp
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Events.Forms as E
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3 as B
import Model.Resource as R
import Network.HTTP.Affjax (AJAX())
import Quasar.Aff as API
import Render.Common
import Render.CssClasses as Rc
import Utils.Path (DirPath(), dropNotebookExt)

type Slam e = Aff (HalogenEffects ( ajax :: AJAX
                                  , now :: Now
                                  , ref :: REF
                                  | e ) )

type StateRec =
  { showList :: Boolean
  , initial :: R.Resource
  , name :: String
  , dirs :: Array R.Resource
  , dir :: DirPath
  , siblings :: Array R.Resource
  , error :: Maybe String
  }

newtype State = State StateRec

initialState :: R.Resource -> State
initialState resource = State
  { showList: false
  , initial: resource
  , name: if R.isNotebook resource
          then dropNotebookExt $ R.resourceName resource
          else R.resourceName resource
  , dir: R.resourceDir resource
  , siblings: mempty
  , dirs: singleton R.root
  , error: Nothing
  }

_State :: LensP State StateRec
_State = lens (\(State obj) -> obj) (const State)

_showList :: LensP State Boolean
_showList = _State <<< lens _.showList (_ { showList = _ })

_initial :: LensP State R.Resource
_initial = _State <<< lens _.initial (_ { initial = _ })

_name :: LensP State String
_name = _State <<< lens _.name (_ { name = _ })

_dirs :: LensP State (Array R.Resource)
_dirs = _State <<< lens _.dirs (_ { dirs = _ })

_dir :: LensP State DirPath
_dir = _State <<< lens _.dir (_ { dir = _ })

_siblings :: LensP State (Array R.Resource)
_siblings = _State <<< lens _.siblings (_ { siblings = _ })

_error :: LensP State (Maybe String)
_error = _State <<< lens _.error (_ { error = _ })

renameSlam :: State -> R.Resource
renameSlam r =
  let initial = r ^. _initial
      name = r ^. _name
      nameWithExt = if R.isNotebook initial
                    then name <> "." <> Config.notebookExtension
                    else name
  in initial # (R._name .~ nameWithExt)
           <<< (R._root .~ (r ^. _dir))

validate :: State -> State
validate r
  | r ^. _initial == renameSlam r = r # _error .~ Nothing
  | otherwise = r # _error .~ either Just (const Nothing) do
    let name = r ^. _name
    when (name == "")
      $ throwError "Please enter a name for the file"

    when (isJust $ S.stripSuffix ("." <> Config.notebookExtension) name)
      $ throwError $ "Please choose an alternative name, ."
      <> Config.notebookExtension
      <> " is a reserved extension"

    when (isJust $ S.indexOf "/" name)
      $ throwError "Please entera valid name for the file"

    let nameWithExt = if R.isNotebook (r ^. _initial)
                      then name <> "." <> Config.notebookExtension
                      else name
    when (isJust $ elemIndex nameWithExt (map (^. R._name) (r ^. _siblings)))
      $ throwError "An item with this name already exists in the target folder"

data Query a
  = Dismiss a
  | SetShowList Boolean a
  | ToggleShowList a
  | Submit a
  | NameTyped String a
  | DirTyped String a
  | DirClicked R.Resource a
  | SetSiblings (Array R.Resource) a
  | AddDirs (Array R.Resource) a
  | Init a

comp :: forall e. Component State Query (Slam e)
comp = component render eval

render :: State -> ComponentHTML Query
render dialog =
  modalDialog
  [ modalHeader "Download"
  , modalBody
    $ H.form [ P.classes [ Rc.renameDialogForm ]
             , Cp.nonSubmit
             , P.initializer (\_ -> action Init)
             , E.onClick (\_ -> E.stopPropagation $> action (SetShowList false))
             ]
    [ nameInput
    , dirDropdownField
    , dirDropdownList
    , errorMessage
    ]
  , modalFooter
    [ H.button [ P.classes [ B.btn ]
               , E.onClick (E.input_ Dismiss)
               ]
      [ H.text "Cancel" ]
    , H.button [ P.classes [ B.btn, B.btnPrimary ]
               , P.disabled $ isJust $ dialog ^. _error
               , E.onClick (E.input_ Submit)
               ]
      [ H.text "Rename" ]
    ]
  ]
  where
  nameInput :: HTML Void Query
  nameInput =
    formGroup [ H.input [ P.classes [ B.formControl ]
                        , P.value (dialog ^. _name)
                        , P.placeholder "New name"
                        , E.onValueInput (E.input NameTyped)
                        ]
              ]

  dirDropdownField :: HTML Void Query
  dirDropdownField =
    H.div [ P.classes [ B.inputGroup ] ]
    [ H.input [ P.classes [ B.formControl ]
              , P.placeholder "New directory"
              , E.onValueInput (E.input DirTyped)
              , P.value (printPath $ dialog ^. _dir)
              ]
    , H.span [ P.classes [ B.inputGroupBtn ] ]
      [ H.button [ P.classes [ B.btn, B.btnDefault ]
                 , E.onClick (\_ -> E.stopPropagation
                                    $> (action ToggleShowList)
                             )
                 ]
        [ H.span [ P.classes [ B.caret ] ] [ ] ]
      ]
    ]
  dirDropdownList :: HTML Void Query
  dirDropdownList =
    H.ul [ P.classes $ [ B.listGroup, Rc.fileListGroup ]
           <> fadeWhen (not $ dialog ^. _showList) ]
    $ renameItem <$> dialog ^. _dirs

  errorMessage :: HTML Void Query
  errorMessage =
    H.div [ P.classes $ [ B.alert, B.alertDanger ]
            <> fadeWhen (isNothing (dialog ^. _error)) ]
    $ maybe [ ] (pure <<< H.text) (dialog ^. _error)

  renameItem :: R.Resource -> HTML Void Query
  renameItem res =
    H.button [ P.classes ([ B.listGroupItem ]
                          <> (if R.isHidden res
                              then [ Rc.itemHidden ]
                              else [ ]))
             , E.onClick (E.input_ (DirClicked res))
             ]
    [ H.text (R.resourcePath res) ]

eval :: forall e. Eval Query State Query (Slam e)
eval (Dismiss next) = pure next
eval (SetShowList bool next) = do
  modify (_showList .~ bool)
  modify validate
  pure next
eval (ToggleShowList next) = do
  modify (_showList %~ not)
  modify validate
  pure next
eval (Submit next) = do
  state <- get
  let src = state ^. _initial
      tgt = R.getPath $ renameSlam state
  result <- liftAff' $ attempt (API.move src tgt)
  case result of
    Left e ->
      modify (_error ?~ message e)
    Right _ -> do
      modify (_error .~ Nothing)
      liftEff' reload
  pure next
eval (NameTyped str next) = do
  modify (_name .~ str)
  modify validate
  pure next
eval (DirTyped str next) = do
  maybe (pure unit) (dirItemClicked <<< R.mkDirectory <<< Right) do
    d <- parseAbsDir str
    s <- sandbox rootDir d
    pure $ rootDir </> s
  pure next
eval (DirClicked res next) = do
  dirItemClicked res
  pure next
eval (SetSiblings ss next) = do
  modify (_siblings .~ ss)
  pure next
eval (AddDirs ds next) = do
  modify (_dirs %~ append ds >>> nub >>> sort)
  pure next
eval (Init next) = do
  state <- get
  dirItemClicked $ R.parent $ state ^. _initial
  pure next

dirItemClicked :: forall e. R.Resource -> Free (HalogenF State Query (Slam e)) Unit
dirItemClicked res =
  case R.getPath res of
    Left _ -> pure unit
    Right dir -> do
      siblings <- liftAff' $ API.children dir
      modify $ (_dir .~ dir)
           <<< (_showList .~ false)
           <<< (_siblings .~ siblings)
