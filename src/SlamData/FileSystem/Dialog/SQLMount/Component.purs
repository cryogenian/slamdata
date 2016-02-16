module SlamData.FileSystem.Dialog.SQLMount.Component where

import Prelude

import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Halogen.Component
  (AceQuery(..), AceState(), Autocomplete(..), aceConstructor)
import Ace.Types (Editor())
import Control.Coroutine.Aff (produce)
import Control.Coroutine.Stalling as SCR
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception as Exn
import Data.Array as Arr
import Data.Either as E
import Data.Foldable as F
import Data.Functor (($>))
import Data.Functor.Aff (liftAff)
import Data.Functor.Coproduct (Coproduct())
import Data.Functor.Eff (liftEff)
import Data.Lens (LensP(), lens, (?~), (%~), (.~))
import Data.Maybe as M
import Data.Path.Pathy as Pt
import Data.StrMap as Sm
import Data.Tuple as Tpl
import Halogen
import Halogen.CustomProps as Cp
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Query.EventSource (EventSource(..))
import Halogen.Themes.Bootstrap3 as B
import Quasar.Aff as Api
import Quasar.Auth as Auth
import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import SlamData.Effects (Slam())
import SlamData.FileSystem.Resource as R
import SlamData.Render.CSS as Rc
import SlamData.Render.Common (fadeWhen)
import Utils.Array (enumerate)
import Utils.Path (DirPath())

type SQLMountDSL = ParentDSL State AceState Query AceQuery Slam Unit
type SQLMountHTML = ParentHTML AceState Query AceQuery Slam Unit
type StateP = InstalledState State AceState Query AceQuery Slam Unit
type QueryP = Coproduct Query (ChildF Unit AceQuery)

type State =
  {
    inProgress :: Boolean
  , vars :: Array (Tpl.Tuple String String)
  , name :: String
  , message :: M.Maybe String
  , path :: DirPath
  , nameFreezed :: Boolean
  }

_inProgress :: forall a r. LensP {inProgress :: a|r} a
_inProgress = lens _.inProgress _{inProgress = _}

_vars :: forall a r. LensP {vars :: a|r} a
_vars = lens _.vars _{vars = _}

_name :: forall a r. LensP {name :: a|r} a
_name = lens _.name _{name = _}

_message :: forall a r. LensP {message :: a|r} a
_message = lens _.message _{message = _}

_nameFreezed :: forall a r. LensP {nameFreezed :: a|r} a
_nameFreezed = lens _.nameFreezed _{nameFreezed = _}

initialState :: DirPath -> State
initialState path =
  {
    inProgress: false
  , vars: [ Tpl.Tuple "" "" ]
  , name: "New sql mount"
  , message: M.Nothing
  , path: path
  , nameFreezed: false
  }

data Query a
  = Dismiss a
  | AddVarPair a
  | EditKey Int String a
  | EditValue Int String a
  | RemoveVarPair Int a
  | UpdateName String a
  | Submit a
  | Success R.Resource a
  | Reload {sql :: String, vars :: Sm.StrMap String } a

comp :: Component StateP QueryP Slam
comp = parentComponent render eval

eval :: Natural Query SQLMountDSL
eval (Dismiss next) = pure next
eval (AddVarPair next) = do
  modify $ _vars %~ flip Arr.snoc (Tpl.Tuple "" "")
  pure next
eval (EditKey i str next) = do
  modify (_vars %~
            M.fromMaybe [] <<< Arr.alterAt i (\(Tpl.Tuple _ s) ->
                                                 pure $ Tpl.Tuple str s))
  pure next
eval (EditValue i str next) = do
  modify (_vars %~
            M.fromMaybe [] <<< Arr.alterAt i (\(Tpl.Tuple f _) ->
                                                 pure $ Tpl.Tuple f str))
  pure next
eval (RemoveVarPair i next) = do
  modify (_vars %~ M.fromMaybe [] <<< Arr.deleteAt i)
  pure next
eval (UpdateName str next) = modify (_name .~ str) $> next
eval (Submit next) = do
  modify $ _inProgress .~ true
  content <- map (M.fromMaybe "") $ query unit $ request GetText
  state <- get
  name <-
    if state.nameFreezed
    then pure state.name
    else liftAff $ Auth.authed $ Api.getNewName state.path state.name
  let
    src = R.Directory state.path
    tgt = R.ViewMount $ state.path Pt.</> Pt.file name
  eiResult <- liftAff $ attempt $ Auth.authed $ Api.portView src tgt content (Sm.fromFoldable state.vars)
  modify $ _inProgress .~ false
  modify $ _name .~ name
  case eiResult of
    E.Left exn -> modify $ _message ?~ Exn.message exn
    _ ->
      subscribe'
        $ EventSource
        $ SCR.producerToStallingProducer
        $ produce \emit -> do
          emit $ E.Left $ Success tgt unit
          emit $ E.Right unit
  pure next
eval (Success _ next) = pure next
eval (Reload r next) = do
  modify $ _nameFreezed .~ true
  modify $ _vars .~ Sm.foldMap (\key val -> [Tpl.Tuple key val]) r.vars
  query unit $ action $ SetText r.sql
  pure next


render :: State -> SQLMountHTML
render state =
  modalDialog
  [
    modalHeader "Mount view"
  , modalBody
    $ H.form
      [
        P.classes [ Rc.sqlMountForm ]
      , Cp.nonSubmit
      ]
      $ [
          H.label_ [ H.text "View mount name" ]
        , nameField state
        , H.label_ [ H.text "SQL² input" ]
        , H.Slot (aceConstructor unit aceSetup (M.Just Live))
        , H.label_ [ H.text "Query variables" ]
        ]
      <> (F.foldMap varPair $ enumerate state.vars)
      <> [
           addVarPairButton
         , errMessage state.message
         ]

  , modalFooter
    [
      progressSpinner state
    , btnCancel
    , btnMount state
    ]
  ]

  where
  errMessage :: M.Maybe String -> SQLMountHTML
  errMessage msg =
    H.div
      [
        P.classes
          (
            [ B.alert, B.alertDanger, B.alertDismissable ]
            <> (fadeWhen $ M.isNothing msg)
          )
      ]
      $ M.maybe [ ] (pure <<< H.text) msg

  aceSetup :: Editor -> Slam Unit
  aceSetup editor = liftEff do
    Editor.setMinLines 6 editor
    Editor.setMaxLines 10000 editor
    Editor.setAutoScrollEditorIntoView true editor
    Editor.setTheme "ace/theme/chrome" editor
    Editor.setEnableLiveAutocompletion true editor
    Editor.setEnableBasicAutocompletion true editor
    session <- Editor.getSession editor
    Session.setMode "ace/mode/sql" session


  nameField state =
    H.input
      [
        P.classes [ B.formControl ]
      , P.value state.name
      , P.disabled state.nameFreezed
      , E.onValueInput (E.input $ UpdateName)
      ]

  varPair (Tpl.Tuple i (Tpl.Tuple key val)) =
    [
      H.div [ P.classes [ B.row, Rc.sqlMountVarPair ] ]
        [
          H.div
            [
              P.classes [ B.colXs5, B.formGroup ]
            ]
            [ H.label_
              [ H.text "Name: "
              , H.input
                  [
                    P.value key
                  , P.classes [ B.formControl ]
                  , E.onValueInput (E.input $ EditKey i)
                  ]
              ]
            ]
        , H.div
            [
              P.classes [ B.colXs5, B.formGroup ]
            ]
            [
              H.label_
                [ H.text "Value: "
                , H.input
                    [
                      P.value val
                    , P.classes [ B.formControl ]
                    , E.onValueInput (E.input $ EditValue i)
                    ]
                ]
            ]
        , H.div
            [
              P.classes [ B.colXs2 ]
            ]
            [
              H.button
                [
                  E.onClick (E.input_ $ RemoveVarPair i)
                , P.classes [ B.btn ]
                ]
                [
                  H.i [ P.classes [ B.glyphicon, B.glyphiconRemove ] ] [ ]
                ]
            ]
        ]
    ]
  addVarPairButton =
    H.button
      [
        P.classes [ B.btn, B.btnPrimary, Rc.sqlMountAddVarPairButton ]
      , E.onClick (E.input_ AddVarPair)
      ]
      [ H.i [ P.classes [ B.glyphiconPlus, B.glyphicon ] ] [ ] ]

progressSpinner :: State -> SQLMountHTML
progressSpinner state =
  H.img
    [ P.src "img/spin.gif"
    , P.class_ $ Rc.mountProgressSpinner state.inProgress
    ]

btnCancel :: SQLMountHTML
btnCancel =
  H.button
    [ P.classes [ B.btn ]
    , E.onClick (E.input_ Dismiss)
    ]
    [ H.text "Cancel" ]

btnMount :: State -> SQLMountHTML
btnMount state =
  H.button
    [ P.classes [ B.btn, B.btnPrimary ]
    , P.disabled (state.inProgress || not (validate state))
    , E.onClick (E.input_ Submit)
    ]
    [ H.text text ]
  where
  text :: String
  text = if state.nameFreezed
         then "Alter view mount"
         else "Mount view"

  validate :: State -> Boolean
  validate state = true
