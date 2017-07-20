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

module SlamData.FileSystem.Dialog.Download.Component
  ( Query
  , Message
  , initialButtons
  , component
  ) where

import SlamData.Prelude

import Control.Monad.State (state)
import Data.Lens (_Left, _Right, (.~))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Dialog.Component as Dialog
import SlamData.Download.Model as DM
import SlamData.Download.Render as DR
import SlamData.FileSystem.Dialog.Download.Component.State as S
import SlamData.FileSystem.Resource as R
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN

data Query a = Modify (∀ r. DM.DownloadModel r → DM.DownloadModel r) a

type Message = Dialog.InnerMessage (DM.DownloadModel ())

initialButtons ∷ R.Resource → Dialog.Buttons (DM.DownloadModel ())
initialButtons = S.buttonsFromState ∘ S.initialState

component ∷ R.Resource → H.Component HH.HTML Query Unit Message Slam
component res =
  H.component
    { initialState: const (S.initialState res)
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ S.State → H.ComponentHTML Query
render state@{ options, targetName } =
  HH.div_
    [ HH.div
        [ HP.classes [ CN.formGroup, CN.downloadSource ] ]
        [ HH.label_
            [ HH.span_ [ HH.text "Source" ]
            , HH.text (R.resourcePath state.resource)
            ]
        ]
    , DR.fldName
        (DM.shouldCompress state)
        options
        targetName
        (\name → Modify (_ { targetName = name }))
    , renderOptions state
    , renderError state
    ]

renderOptions ∷ S.State → H.ComponentHTML Query
renderOptions { options } =
  HH.div
    [ HP.classes [ CN.formGroup ] ]
    [ HH.ul
        [ HP.classes [ CN.nav, CN.navTabs ] ]
        [ HH.li
            (guard (isLeft options) $> HP.class_ CN.active)
            [ HH.a
                [ HE.onClick $ HE.input_ (Modify (setOutput DM.CSV)) ]
                [ HH.text "CSV" ]
            ]
        , HH.li
            (guard (isRight options) $> HP.class_ CN.active)
            [ HH.a
                [ HE.onClick $ HE.input_ (Modify (setOutput DM.JSON)) ]
                [ HH.text "JSON" ]
            ]
        ]
    , either renderOptionsCSV renderOptionsJSON options
    ]

setOutput ∷ ∀ r. DM.OutputType → DM.DownloadModel r → DM.DownloadModel r
setOutput ty st = case ty, st.options of
  DM.CSV, Left _ → st
  DM.CSV, _ → st { options = Left DM.initialCSVOptions }
  DM.JSON, Right _ → st
  DM.JSON, _ → st { options = Right DM.initialJSONOptions }

renderOptionsCSV ∷ DM.CSVOptions → H.ComponentHTML Query
renderOptionsCSV =
  DR.optionsCSV \lens v →
    Modify \st → st { options = st.options # _Left ∘ lens .~ v }

renderOptionsJSON ∷ DM.JSONOptions → H.ComponentHTML Query
renderOptionsJSON =
  DR.optionsJSON \lens v →
    Modify \st → st { options = st.options # _Right ∘ lens .~ v }

renderError ∷ S.State → H.ComponentHTML Query
renderError { error } = case error of
  Nothing → HH.text ""
  Just err →
    HH.div
      [ HP.classes [ CN.alert, CN.alertDanger ] ]
      [ HH.text err ]

eval ∷ Query ~> H.ComponentDSL S.State Query Message Slam
eval (Modify f next) = do
  newState ← state (\st → let st' = S.validate (f st) in st' × st')
  H.raise (Dialog.Change (S.buttonsFromState newState))
  pure next
