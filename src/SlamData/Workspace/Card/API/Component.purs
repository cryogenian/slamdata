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

module SlamData.Workspace.Card.API.Component
  ( apiComponent
  , queryShouldRun
  ) where

import SlamData.Prelude

import Control.Monad.Error.Class as EC

import Data.List as L
import Data.StrMap as SM

import Halogen as H
import Halogen.HTML.Indexed as HH

import SlamData.Effects (Slam)
import SlamData.Workspace.Card.API.Component.Query (QueryP)
import SlamData.Workspace.Card.API.Component.State (State, initialState)
import SlamData.Workspace.Card.API.Model as Model
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Common.EvalQuery (runCardEvalT)
import SlamData.Workspace.Card.Component as NC
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.FormBuilder.Component as FB
import SlamData.Workspace.FormBuilder.Item.Component as Item

type APIHTML = H.ParentHTML (FB.StateP Slam) NC.CardEvalQuery FB.QueryP Slam Unit
type APIDSL = H.ParentDSL State (FB.StateP Slam) NC.CardEvalQuery FB.QueryP Slam Unit

apiComponent ∷ H.Component NC.CardStateP NC.CardQueryP Slam
apiComponent =
  NC.makeCardComponent
    { cardType: CT.API
    , component: H.parentComponent { render, eval, peek: Nothing }
    , initialState: H.parentState initialState
    , _State: NC._APIState
    , _Query: NC.makeQueryPrism NC._APIQuery
    }

render
  ∷ State
  → APIHTML
render _ =
  HH.slot unit \_ →
   { component : FB.formBuilderComponent
   , initialState : H.parentState FB.initialState
   }

compileVarMap
  ∷ L.List Item.Model
  → Port.VarMap
  → Port.VarMap
compileVarMap fields globalVarMap =
  foldl alg SM.empty fields
  where
    alg =
      flip \{ name, fieldType, defaultValue } →
        maybe id (SM.insert name) $
          SM.lookup name globalVarMap
            <|> (Item.defaultValueToVarMapValue fieldType =<< defaultValue)

eval ∷ Natural NC.CardEvalQuery APIDSL
eval q =
  case q of
    NC.EvalCard info k →
      k <$> runCardEvalT do
        fields ←
          H.query unit (H.request (FB.GetItems ⋙ left))
            # lift
            >>= maybe (EC.throwError "Error querying FormBuilder") pure
        pure ∘ Just $ Port.VarMap $ compileVarMap fields info.globalVarMap
    NC.SetupCard _ next →
      pure next
    NC.NotifyRunCard next →
      pure next
    NC.NotifyStopCard next →
      pure next
    NC.Save k →
      H.query unit (H.request (FB.GetItems ⋙ left)) <#>
        maybe [] L.fromList
          ⋙ { items : _ }
          ⋙ Model.encode
          ⋙ k
    NC.Load json next → do
      for_ (Model.decode json) \{items} →
        H.query unit $ H.action (FB.SetItems (L.toList items) ⋙ left)
      pure next
    NC.SetCanceler _ next → pure next
    NC.SetDimensions _ next → pure next

queryShouldRun
  ∷ forall a
   . QueryP a
  → Boolean
queryShouldRun =
  coproduct
    (const false)
    (H.runChildF ⋙
       coproduct
         (const false)
         (H.runChildF ⋙ pred))

  where
    pred q =
      case q of
        Item.Update _ → true
        _ → false
