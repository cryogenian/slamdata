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
  , QueryP
  , StateP
  , ChildState
  , ChildQuery
  , ChildSlot
  , module SlamData.FileSystem.Dialog.Mount.Component.Query
  , module SlamData.FileSystem.Dialog.Mount.Component.State
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Exception (Error, message)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)

import Data.Argonaut (jsonParser, decodeJson, (.?))
import Data.Lens (set, (.~), (?~))

import Ace.Halogen.Component (AceQuery(..))

import Halogen as H
import Halogen.Component.ChildPath (ChildPath, cpL, cpR)
import Halogen.CustomProps as CP
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B
import Halogen.Component.Utils (forceRerender')

import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import SlamData.Effects (Slam)
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery as SQ
import SlamData.FileSystem.Dialog.Mount.Component.Query (Query(..))
import SlamData.FileSystem.Dialog.Mount.Component.State (MountSettings, State, _message, _name, _new, _parent, _saving, _settings, canSave, initialSettings, initialState, scheme, validate)
import SlamData.FileSystem.Dialog.Mount.MongoDB.Component as MongoDB
import SlamData.FileSystem.Dialog.Mount.Scheme (Scheme(..), schemes, schemeToString, schemeFromString)
import SlamData.FileSystem.Dialog.Mount.SQL2.Component as SQL2
import SlamData.Quasar.FS as Api
import SlamData.Render.CSS as Rc

type ChildState = Either SQL2.StateP MongoDB.State
type ChildQuery = Coproduct SQL2.QueryP MongoDB.Query
type ChildSlot = Either Unit Unit

cpSQL :: ChildPath SQL2.StateP ChildState SQL2.QueryP ChildQuery Unit ChildSlot
cpSQL = cpL

cpMongoDB :: ChildPath MongoDB.State ChildState MongoDB.Query ChildQuery Unit ChildSlot
cpMongoDB = cpR

type DSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot
type HTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot

type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot
type QueryP = Coproduct Query (H.ChildF ChildSlot ChildQuery)

comp :: H.Component StateP QueryP Slam
comp = H.parentComponent { render, eval, peek: Just (peek <<< H.runChildF) }

render :: State -> HTML
render state@{ new } =
  modalDialog
    [ modalHeader "Mount"
    , modalBody $
        HH.form
          [ CP.nonSubmit, HP.class_ Rc.dialogMount ]
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
      HH.slot' cpMongoDB unit \_ ->
        { component: MongoDB.comp, initialState }
    Right initialState ->
      HH.slot' cpSQL unit \_ ->
        { component: SQL2.comp, initialState: H.parentState initialState }

fldName :: State -> HTML
fldName state =
  HH.div
    [ HP.classes [B.formGroup, Rc.mountName] ]
    [ HH.label_
        [ HH.span_ [ HH.text "Name" ]
        , HH.input
            [ HP.class_ B.formControl
            , HE.onValueInput $ HE.input (ModifyState <<< set _name)
            , HP.value (state.name)
            ]
        ]
    ]

selScheme :: State -> HTML
selScheme state =
  HH.div
    [ HP.class_ B.formGroup ]
    [ HH.label_
        [ HH.span_ [ HH.text "Mount type" ]
        , HH.select
            [ HP.class_ B.formControl
            , HE.onValueChange (HE.input SelectScheme <<< schemeFromString)
            ]
            $ [ HH.option_ [] ] ++ schemeOptions
        ]
    ]
  where
  schemeOptions = map (\s -> HH.option_ [ HH.text (schemeToString s) ]) schemes

errorMessage :: String -> HTML
errorMessage msg =
  HH.div
    [ HP.classes [ B.alert, B.alertDanger ] ]
    [ HH.text msg ]

btnCancel :: HTML
btnCancel =
  HH.button
    [ HP.classes [B.btn]
    , HE.onClick (HE.input_ Dismiss)
    ]
    [ HH.text "Cancel" ]

btnMount :: State -> HTML
btnMount state@{ new, saving } =
  HH.button
    [ HP.classes [B.btn, B.btnPrimary]
    , HP.enabled (not saving && canSave state)
    , HE.onClick (HE.input_ NotifySave)
    ]
    [ HH.text text ]
  where
  text = if new then "Mount" else "Save changes"

progressSpinner :: State -> HTML
progressSpinner { saving } =
  HH.img [ HP.src "img/spin.gif", HP.class_ (Rc.mountProgressSpinner saving) ]

eval :: Natural Query DSL
eval (ModifyState f next) = H.modify f *> validateInput $> next
eval (SelectScheme newScheme next) = do
  currentScheme <- map scheme <$> H.gets _.settings
  when (currentScheme /= newScheme) do
    H.modify (_settings .~ map initialSettings newScheme)
    forceRerender'
    validateInput
  pure next
eval (Dismiss next) = pure next
eval (NotifySave next) = pure next
eval (Save k) = do
  { parent, name, new } <- H.get
  H.modify (_saving .~ true)
  newName <-
    if new then Api.getNewName parent name else pure (pure name)
  case newName of
    Left err → do
      H.modify (_message ?~ formatError err)
      pure $ k Nothing
    Right newName' -> do
      result <- querySettings (H.request (SQ.Submit parent newName'))
      mount <- case result of
        Just (Right m) -> pure (Just m)
        Just (Left err) -> H.modify (_message ?~ formatError err) $> Nothing
        Nothing -> pure Nothing
      H.modify (_saving .~ false)
      pure $ k mount

peek :: forall x. ChildQuery x -> DSL Unit
peek = coproduct (coproduct peekSQ (peekAce <<< H.runChildF)) peekSQ
  where
  peekSQ :: forall s. SQ.SettingsQuery s x -> DSL Unit
  peekSQ (SQ.ModifyState _ _ ) = validateInput
  peekSQ _ = pure unit
  peekAce :: AceQuery x -> DSL Unit
  peekAce (TextChanged _) = validateInput
  peekAce _ = pure unit

validateInput :: DSL Unit
validateInput = do
  state <- H.get
  message <- runExceptT do
    liftMaybe (validate state)
    liftMaybe <<< join =<< lift (querySettings (H.request SQ.Validate))
  H.modify (_message .~ either Just (const Nothing) message)
  where
  liftMaybe :: Maybe String -> ExceptT String DSL Unit
  liftMaybe = maybe (pure unit) throwError

formatError :: Error -> String
formatError err =
  "There was a problem saving the mount: " <> extract (message err)
  where
  extract msg =
    either (const msg) id (jsonParser msg >>= decodeJson >>= (_ .? "error"))

querySettings :: forall a. (forall s. SQ.SettingsQuery s a) -> DSL (Maybe a)
querySettings q = (map scheme <$> H.gets _.settings) >>= \s ->
  case s of
    Just MongoDB -> H.query' cpMongoDB unit q
    Just SQL2 -> H.query' cpSQL unit (left q)
    _ -> pure Nothing
