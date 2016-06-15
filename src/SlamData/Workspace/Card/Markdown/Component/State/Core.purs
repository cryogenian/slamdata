module SlamData.Workspace.Card.Markdown.Component.State.Core where

import Control.Monad.Eff.Class (class MonadEff)

import SlamData.Prelude
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.Markdown.Interpret as MDI

import Data.BrowserFeatures (BrowserFeatures)
import Data.Date.Locale as DL
import Data.StrMap as SM

import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Halogen.Component as SDH

type State =
  { browserFeatures ∷ Maybe BrowserFeatures
  , input ∷ Maybe (SD.SlamDownP VM.VarMapValue)
  }

initialState ∷ State
initialState =
  { browserFeatures: Nothing
  , input: Nothing
  }

formStateToVarMap
  ∷ ∀ m e
  . (MonadEff (locale ∷ DL.Locale | e) m, Applicative m)
  ⇒ SDH.SlamDownFormDesc VM.VarMapValue
  → SDH.SlamDownFormState VM.VarMapValue
  → m VM.VarMap
formStateToVarMap desc st =
  SM.foldM
    (\m k field → do
       v ← valueForKey k field
       pure $ SM.insert k v m)
    SM.empty
    desc

  where
    valueForKey
      ∷ ∀ f a
      . String
      → SD.FormFieldP f a
      → m VM.VarMapValue
    valueForKey k field =
      fromMaybe (MDI.formFieldEmptyValue field) <$>
        case SM.lookup k st of
          Just v → MDI.formFieldValueToVarMapValue v
          Nothing → pure Nothing
