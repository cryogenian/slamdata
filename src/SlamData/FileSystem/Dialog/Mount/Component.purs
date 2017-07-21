{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
embedu may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.FileSystem.Dialog.Mount.Component
  ( Query
  , initialButtons
  , component
  , module S
  , module MA
  ) where

import SlamData.Prelude

import Control.Coroutine as CR
import Control.Monad.Aff.AVar as AV
import Control.Monad.Eff.Exception as Exn
import Data.Array as A
import Data.Lens (Prism', preview, review)
import Data.Map as M
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Component.Utils as HCU
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Quasar.Mount as QM
import SlamData.Dialog.Component as Dialog
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery as SQ
import SlamData.FileSystem.Dialog.Mount.Component.ChildSlot as CS
import SlamData.FileSystem.Dialog.Mount.Component.State as S
import SlamData.FileSystem.Dialog.Mount.Couchbase.Component as Couchbase
import SlamData.FileSystem.Dialog.Mount.MarkLogic.Component as MarkLogic
import SlamData.FileSystem.Dialog.Mount.MongoDB.Component as MongoDB
import SlamData.FileSystem.Dialog.Mount.MountAction as MA
import SlamData.FileSystem.Dialog.Mount.Scheme (Scheme)
import SlamData.FileSystem.Dialog.Mount.Scheme as MS
import SlamData.FileSystem.Dialog.Mount.SparkFTP.Component as SparkFTP
import SlamData.FileSystem.Dialog.Mount.SparkHDFS.Component as SparkHDFS
import SlamData.FileSystem.Dialog.Mount.SparkLocal.Component as SparkLocal
import SlamData.FileSystem.Dialog.Mount.SQL2.Component as SQL2
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN

data Query a
  = Init a
  | SetName String a
  | SelectScheme (Maybe Scheme) a
  | Update (SQ.SettingsMessage QM.MountConfig) a
  | SetErrorMessage String a

type Message = Dialog.InnerMessage MA.MountAction

type DSL = H.ParentDSL S.State Query CS.ChildQuery CS.ChildSlot Message Slam
type HTML = H.ParentHTML Query CS.ChildQuery CS.ChildSlot Slam

initialButtons ∷ S.Input → Dialog.Buttons MA.MountAction
initialButtons = S.buttonsFromState ∘ S.initialState

component ∷ S.Input → H.Component HH.HTML Query Unit Message Slam
component input =
  H.lifecycleParentComponent
    { initialState: const (S.initialState input)
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }

render ∷ S.State → HTML
render state@{ input, name, scheme, errors } =
  HH.div
    [ HP.class_ (H.ClassName "sd-mount-dialog-inner") ]
    $ join
        [ foldMap (pure ∘ renderNameField) name
        , pure $ renderSelectScheme scheme
        , foldMap (pure ∘ renderSettings (S.originalMount state.input)) state.scheme
        , foldMap (pure ∘ renderErrorMessage) $ snd <$> A.head (M.toAscUnfoldable errors)
        ]

renderSettings ∷ Maybe QM.MountConfig → MS.Scheme → HTML
renderSettings config =
  case _ of
    MS.MongoDB → embed CS.cpMongoDB MongoDB.component QM._MongoDB
    MS.SQL2 → embed CS.cpSQL SQL2.component QM._View
    MS.Couchbase → embed CS.cpCouchbase Couchbase.component QM._Couchbase
    MS.MarkLogic → embed CS.cpMarkLogic MarkLogic.component QM._MarkLogic
    MS.SparkFTP → embed CS.cpSparkFTP SparkFTP.component QM._SparkFTP
    MS.SparkHDFS → embed CS.cpSparkHDFS SparkHDFS.component QM._SparkHDFS
    MS.SparkLocal → embed CS.cpSparkLocal SparkLocal.component QM._SparkLocal
  where
    embed
      ∷ ∀ f a
      . CP.ChildPath f CS.ChildQuery Unit CS.ChildSlot
      → H.Component HH.HTML f (Maybe a) (SQ.SettingsMessage a) Slam
      → Prism' QM.MountConfig a
      → HTML
    embed cp comp prism =
      HH.slot' cp unit comp
        (preview prism =<< config)
        (HE.input Update ∘ map (review prism))

renderNameField ∷ String → HTML
renderNameField name =
  HH.div
    [ HP.class_ CN.formGroup ]
    [ HH.label_
        [ HH.span_ [ HH.text "Name" ]
        , HH.input
            [ HP.class_ CN.formControl
            , HE.onValueInput (HE.input SetName)
            , HP.value name
            ]
        ]
    ]

renderSelectScheme ∷ Maybe MS.Scheme → HTML
renderSelectScheme scheme =
  HH.div
    [ HP.class_ CN.formGroup ]
    [ HH.label_
        [ HH.span_ [ HH.text "Mount type" ]
        , HH.select
            [ HP.class_ CN.formControl
            , HE.onValueChange (HE.input SelectScheme ∘ MS.schemeFromString)
            ]
            $ [ HH.option_ [] ] <> map renderOption MS.schemes
        ]
    ]
  where
    renderOption s =
      HH.option
        [ HP.selected (scheme == Just s) ]
        [ HH.text (MS.schemeToString s) ]

renderErrorMessage ∷ String → HTML
renderErrorMessage msg =
  HH.div
    [ HP.classes [ CN.alert, CN.alertDanger ] ]
    [ HH.text msg ]

eval ∷ Query ~> DSL
eval = case _ of
  Init next → do
    v ← H.liftAff AV.makeVar
    let errorReply = H.liftAff ∘ AV.putVar v
    H.subscribe $ ES.EventSource $ pure
      { producer: CR.producer $ H.liftAff $
          Left ∘ (SetErrorMessage <@> ES.Listening) <$> AV.takeVar v
      , done: H.liftAff $ AV.killVar v (Exn.error "Finalized")
      }
    st ← HCU.modify (_ { errorReply = errorReply })
    H.raise (Dialog.Change (S.buttonsFromState st))
    pure next
  SetName newName next → do
    st ← HCU.modify \st → st
      { name = Just newName
      , errors = M.alter (const (S.validateName newName)) S.NameError st.errors
      }
    H.raise (Dialog.Change (S.buttonsFromState st))
    pure next
  SelectScheme newScheme next → do
    { scheme, input } ← H.get
    when (scheme /= newScheme) do
      let value = flip S.matchSchemeConfig input =<< newScheme
      st ← HCU.modify \st → st
        { scheme = newScheme
        , value = value
        , errors = M.delete S.InnerError (M.delete S.OuterError st.errors)
        }
      H.raise (Dialog.Change (S.buttonsFromState st))
    pure next
  Update (SQ.Modified esm) next → do
    st ← case esm of
      Left err →
        HCU.modify \st → st
          { value = Nothing
          , errors = M.insert S.InnerError err (M.delete S.OuterError st.errors)
          }
      Right v →
        HCU.modify \st → st
          { value = Just v
          , errors = M.delete S.InnerError (M.delete S.OuterError st.errors)
          }
    H.raise (Dialog.Change (S.buttonsFromState st))
    pure next
  SetErrorMessage msg next → do
    H.modify \st → st { errors = M.insert S.OuterError msg st.errors }
    H.raise (Dialog.TogglePending false)
    pure next
