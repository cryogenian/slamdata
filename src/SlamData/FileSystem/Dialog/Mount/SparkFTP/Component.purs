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

module SlamData.FileSystem.Dialog.Mount.SparkFTP.Component
  ( component
  , Query
  , module Q
  , module S
  ) where

import SlamData.Prelude

import Data.Array as Array
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Quasar.Mount.SparkFTP as QMS
import SlamData.FileSystem.Dialog.Mount.Common.Render as MCR
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery as Q
import SlamData.FileSystem.Dialog.Mount.SparkFTP.Component.State as S
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN

type Query = Q.SettingsQuery S.State
type Message = Q.SettingsMessage QMS.Config

component ∷ H.Component HH.HTML Query (Maybe QMS.Config) Message Slam
component =
  H.component
    { initialState: maybe S.initialState S.fromConfig
    , render
    , eval: Q.eval (S.vToE ∘ S.toConfig)
    , receiver: const Nothing
    }

render ∷ S.State → H.ComponentHTML Query
render state =
  HH.div_
    [ MCR.section "Spark Server" [ MCR.host state S._sparkHost ]
    , MCR.section "FTP Server" [ MCR.host state S._ftpHost ]
    , MCR.section "Authentication"
        [ HH.div
            [ HP.class_ CN.mountUserInfo ]
            [ MCR.label "Username" [ MCR.input state S._user [] ]
            , MCR.label "Password" [ MCR.input state S._password [ HP.type_ HP.InputPassword ] ]
            ]
        ]
    , MCR.section "Root" [ MCR.label "Path" [ MCR.input state S._path [] ] ]
    , MCR.section "Advanced Settings"
        [ MCR.propListTable (Array.mapWithIndex renderPropRow (state.props <> [ "" × "" ])) ]
    ]
  where
  renderPropRow ∷ Int → String × String → H.ComponentHTML Query
  renderPropRow ix (key × value) =
    let
      values =
        (guard (key ≠ "") $> "")
          <> [ key ]
          <> state.availableProps
      updateFnKey = S.updatePropAt ix ∘ flip Tuple value
      updateFnVal = S.updatePropAt ix ∘ Tuple key
      classes = [ CN.formControl ]
    in
    HH.tr_
      [ HH.td_
          [ HH.select
              [ HP.value key
              , HP.classes classes
              , HE.onSelectedIndexChange (map (H.action ∘ Q.ModifyState ∘ updateFnKey) ∘ Array.index values)
              ]
              (values <#> HH.option_ ∘ pure ∘ HH.text)
          ]
      , HH.td_
          [ HH.input
              [ HP.value value
              , HP.type_ HP.InputText
              , HP.classes classes
              , HE.onValueInput (HE.input (Q.ModifyState ∘ updateFnVal))
              ]
          ]
      ]
