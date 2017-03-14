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

module SlamData.Workspace.Card.Setups.ActionSelect.Component where

import SlamData.Prelude
import Data.Array as Array
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Monad (Slam)
import SlamData.ActionList.Component as ALC
import SlamData.Workspace.Card.Setups.Dialog as CSD

data Query s a
  = Init a
  | HandleSelect (ALC.Message (Maybe s)) a
  | HandleConfirm (Maybe s) a
  | HandleDismiss a
  | UpdateDimensions a

data Message s
  = Dismiss
  | Confirm (Maybe s)

type State s =
  { options ∷ Array s
  , selection ∷ Maybe s
  , title ∷ String
  , label ∷ s → String
  }

type DSL s = H.ParentDSL (State s) (Query s) (ALC.Query (Maybe s)) Unit (Message s) Slam
type HTML s = H.ParentHTML (Query s) (ALC.Query (Maybe s)) Unit Slam

selectConf ∷ ∀ s. Eq s ⇒ ALC.MkConf (Maybe s)
selectConf = map updateConf <$> ALC.defaultConf
  where
  updateConf conf = conf
    { buttons = _ { presentation = ALC.TextOnly } <$> conf.buttons
    , classes = []
    }

component ∷ ∀ s. Eq s ⇒ H.Component HH.HTML (Query s) (State s) (Message s) Slam
component =
  H.lifecycleParentComponent
    { render
    , eval
    , initialState: id
    , receiver: const Nothing
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }
  where
  render ∷ State s → HTML s
  render st =
    let
      isNull = Array.null st.options
      content =
        if isNull
          then [ HH.p [ HP.classes [ HH.ClassName "no-options" ] ] [ HH.text "No available options" ] ]
          else [ HH.slot unit (ALC.actionListComp selectConf []) unit (HE.input HandleSelect) ]
    in
      CSD.pickerDialog
        { onDismiss: HandleDismiss
        , onConfirm: HandleConfirm
        , selection: Just st.selection
        , isSelectable: const (not isNull)
        , classes: [ HH.ClassName "sd-action-select" ]
        , title: [ HH.text st.title ]
        , content
        }

  eval ∷ Query s ~> DSL s
  eval = case _ of
    Init next → do
      updateActions
      pure next
    HandleSelect (ALC.Selected selection) next → do
      st ← H.get
      if st.selection ≡ selection
        then H.modify _ { selection = Nothing }
        else H.modify _ { selection = selection }
      updateActions
      pure next
    HandleDismiss next →
      H.raise Dismiss $> next
    HandleConfirm selection next →
      H.raise (Confirm selection) $> next
    UpdateDimensions next →
      H.query unit (H.action ALC.CalculateBoundingRect) $> next

mkAction ∷ forall s. Eq s ⇒ Maybe s → (s → String) → s → ALC.Action (Maybe s)
mkAction selection label action =
  ALC.mkDo
    { name: label action
    , description: label action
    , iconSrc: ""
    , highlighted: selection ≡ Just action
    , disabled: false
    , action: Just action
    }

updateActions ∷ ∀ s. Eq s ⇒ DSL s Unit
updateActions = do
  st ← H.get
  void $ H.query unit $ H.action $ ALC.UpdateActions (mkAction st.selection st.label <$> st.options)
