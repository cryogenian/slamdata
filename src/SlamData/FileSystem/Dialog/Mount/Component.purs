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

module SlamData.FileSystem.Dialog.Mount.Component
  ( comp
  , QueryP()
  , StateP()
  , ChildState()
  , ChildQuery()
  , ChildSlot()
  , module SlamData.FileSystem.Dialog.Mount.Component.Query
  , module SlamData.FileSystem.Dialog.Mount.Component.State
  ) where

import Prelude

import Control.Apply ((*>))
import Control.Bind (join, (=<<))
import Control.Monad (when)
import Control.Monad.Eff.Exception (Error(), message)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT(), runExceptT)
import Control.Monad.Trans (lift)
import Control.MonadPlus (guard)

import Data.Argonaut (jsonParser, decodeJson, (.?))
import Data.Either (Either(..), either)
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct(), coproduct, left)
import Data.Functor.Aff (liftAff)
import Data.Lens (set, (.~), (?~))
import Data.Maybe (Maybe(..), maybe)

import Ace.Halogen.Component (AceQuery(..))

import Halogen hiding (HTML(), set)
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR)
import Halogen.CustomProps as CP
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B
import Halogen.Component.Utils (forceRerender')

import Quasar.Aff as Api
import Quasar.Auth as Auth

import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import SlamData.Effects (Slam())
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery as SQ
import SlamData.FileSystem.Dialog.Mount.Component.Query
import SlamData.FileSystem.Dialog.Mount.Component.State
import SlamData.FileSystem.Dialog.Mount.MongoDB.Component as MongoDB
import SlamData.FileSystem.Dialog.Mount.Scheme (Scheme(..), schemes, schemeToString, schemeFromString)
import SlamData.FileSystem.Dialog.Mount.SQL2.Component as SQL2
import SlamData.Render.CSS as Rc

type ChildState = Either SQL2.StateP MongoDB.State
type ChildQuery = Coproduct SQL2.QueryP MongoDB.Query
type ChildSlot = Either Unit Unit

cpSQL :: ChildPath SQL2.StateP ChildState SQL2.QueryP ChildQuery Unit ChildSlot
cpSQL = cpL

cpMongoDB :: ChildPath MongoDB.State ChildState MongoDB.Query ChildQuery Unit ChildSlot
cpMongoDB = cpR

type DSL = ParentDSL State ChildState Query ChildQuery Slam ChildSlot
type HTML = ParentHTML ChildState Query ChildQuery Slam ChildSlot

type StateP = InstalledState State ChildState Query ChildQuery Slam ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)

comp :: Component StateP QueryP Slam
comp = parentComponent' render eval (peek <<< runChildF)

render :: State -> HTML
render state@{ new } =
  modalDialog
    [ modalHeader "Mount"
    , modalBody $
        H.form
          [ CP.nonSubmit, P.class_ Rc.dialogMount ]
          $ (guard new $> fldName state)
          <> (guard new $> selScheme state)
          <> maybe [] (pure <<< settings) state.settings
          <> maybe [] (pure <<< errorMessage) state.message
    , modalFooter
        [ progressSpinner state
        , btnCancel
        , btnMount state
        ]
    ]
  where
  settings :: MountSettings -> HTML
  settings ss = case ss of
    Left initialState ->
      H.slot' cpMongoDB unit \_ ->
        { component: MongoDB.comp, initialState }
    Right initialState ->
      H.slot' cpSQL unit \_ ->
        { component: SQL2.comp, initialState: installedState initialState }

fldName :: State -> HTML
fldName state =
  H.div
    [ P.classes [B.formGroup, Rc.mountName] ]
    [ H.label_
        [ H.span_ [ H.text "Name" ]
        , H.input
            [ P.class_ B.formControl
            , E.onValueInput $ E.input (ModifyState <<< set _name)
            , P.value (state.name)
            ]
        ]
    ]

selScheme :: State -> HTML
selScheme state =
  H.div
    [ P.class_ B.formGroup ]
    [ H.label_
        [ H.span_ [ H.text "Mount type" ]
        , H.select
            [ P.class_ B.formControl
            , E.onValueChange (E.input SelectScheme <<< schemeFromString)
            ]
            $ [ H.option_ [] ] ++ schemeOptions
        ]
    ]
  where
  schemeOptions = map (\s -> H.option_ [ H.text (schemeToString s) ]) schemes

errorMessage :: String -> HTML
errorMessage msg =
  H.div
    [ P.classes [ B.alert, B.alertDanger ] ]
    [ H.text msg ]

btnCancel :: HTML
btnCancel =
  H.button
    [ P.classes [B.btn]
    , E.onClick (E.input_ Dismiss)
    ]
    [ H.text "Cancel" ]

btnMount :: State -> HTML
btnMount state@{ new, saving } =
  H.button
    [ P.classes [B.btn, B.btnPrimary]
    , P.enabled (not saving && canSave state)
    , E.onClick (E.input_ NotifySave)
    ]
    [ H.text text ]
  where
  text = if new then "Mount" else "Save changes"

progressSpinner :: State -> HTML
progressSpinner { saving } =
  H.img [ P.src "img/spin.gif", P.class_ (Rc.mountProgressSpinner saving) ]

eval :: Natural Query DSL
eval (ModifyState f next) = modify f *> validateInput $> next
eval (SelectScheme newScheme next) = do
  currentScheme <- map scheme <$> gets _.settings
  when (currentScheme /= newScheme) do
    modify (_settings .~ map initialSettings newScheme)
    forceRerender'
    validateInput
  pure next
eval (Dismiss next) = pure next
eval (NotifySave next) = pure next
eval (Save k) = do
  { parent, name, new } <- get
  modify (_saving .~ true)
  newName <- liftAff
    if new then Auth.authed (Api.getNewName parent name) else pure name
  result <- querySettings (request (SQ.Submit parent newName))
  mount <- case result of
    Just (Right m) -> pure (Just m)
    Just (Left err) -> modify (_message ?~ formatError err) $> Nothing
    Nothing -> pure Nothing
  modify (_saving .~ false)
  pure $ k mount

peek :: forall x. ChildQuery x -> DSL Unit
peek = coproduct (coproduct peekSQ (peekAce <<< runChildF)) peekSQ
  where
  peekSQ :: forall s. SQ.SettingsQuery s x -> DSL Unit
  peekSQ (SQ.ModifyState _ _ ) = validateInput
  peekSQ _ = pure unit
  peekAce :: AceQuery x -> DSL Unit
  peekAce (TextChanged _) = validateInput
  peekAce _ = pure unit

validateInput :: DSL Unit
validateInput = do
  state <- get
  message <- runExceptT do
    liftMaybe (validate state)
    liftMaybe <<< join =<< lift (querySettings (request SQ.Validate))
  modify (_message .~ either Just (const Nothing) message)
  where
  liftMaybe :: Maybe String -> ExceptT String DSL Unit
  liftMaybe = maybe (pure unit) throwError

formatError :: Error -> String
formatError err =
  "There was a problem saving the mount: " <> extract (message err)
  where
  extract msg =
    either (const msg) id (jsonParser msg >>= decodeJson >>= (.? "error"))

querySettings :: forall a. (forall s. SQ.SettingsQuery s a) -> DSL (Maybe a)
querySettings q = (map scheme <$> gets _.settings) >>= \s ->
  case s of
    Just MongoDB -> query' cpMongoDB unit q
    Just SQL2 -> query' cpSQL unit (left q)
    _ -> pure Nothing
