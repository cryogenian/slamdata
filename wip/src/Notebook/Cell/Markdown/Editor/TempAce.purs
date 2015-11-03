module Notebook.Cell.Markdown.Editor.TempAce where

import Prelude

import Control.Monad (when)

import Data.Maybe (Maybe(..))

import DOM.HTML.Types (HTMLElement())

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Notebook.Common (Slam())

import Ace.Types (Editor())
import Ace.Editor as Editor
import Ace.EditSession as Session

type AceState = { editor :: Maybe Editor }

initAceState :: AceState
initAceState = { editor: Nothing }

data AceQuery a
  = Init HTMLElement a
  | ChangeText String a

aceComponent :: Component AceState AceQuery Slam
aceComponent = component render eval
  where

  render :: AceState -> ComponentHTML AceQuery
  render = const $ H.div [ P.initializer \el -> action (Init el) ] []

  eval :: Natural AceQuery (ComponentDSL AceState AceQuery Slam)
  eval (Init el next) = do
    editor <- liftEff' $ Ace.editNode el Ace.ace
    modify _ { editor = Just editor }
    session <- liftEff' $ Editor.getSession editor
    subscribe $ eventSource_ (Session.onChange session) do
      text <- Editor.getValue editor
      pure $ action (ChangeText text)
    pure next
  eval (ChangeText text next) = do
    state <- gets _.editor
    case state of
      Nothing -> pure unit
      Just editor -> do
        current <- liftEff' $ Editor.getValue editor
        when (text /= current) $ void $ liftEff' $ Editor.setValue text Nothing editor
    pure next
