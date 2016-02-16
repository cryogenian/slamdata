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

module SlamData.Notebook.Cell.API.Component
  ( apiComponent
  , queryShouldRun
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Bind ((=<<))
import Control.Monad.Error.Class as EC
import Control.Monad.Trans as MT

import Data.Foldable as F
import Data.Functor.Coproduct
import Data.List as L
import Data.Maybe as M
import Data.StrMap as SM

import Halogen
import Halogen.HTML.Indexed as H

import SlamData.Notebook.Cell.API.Component.Query
import SlamData.Notebook.Cell.API.Component.State
import SlamData.Notebook.Cell.API.Model as Model
import SlamData.Notebook.Cell.CellType as CT
import SlamData.Notebook.Cell.Common.EvalQuery as NC
import SlamData.Notebook.Cell.Component as NC
import SlamData.Notebook.Cell.Port as Port
import SlamData.Effects (Slam())
import SlamData.Notebook.FormBuilder.Component as FB
import SlamData.Notebook.FormBuilder.Item.Component as Item

type APIHTML = ParentHTML (FB.StateP Slam) NC.CellEvalQuery FB.QueryP Slam Unit
type APIDSL = ParentDSL State (FB.StateP Slam) NC.CellEvalQuery FB.QueryP Slam Unit

apiComponent :: Component NC.CellStateP NC.CellQueryP Slam
apiComponent =
  NC.makeEditorCellComponent
    { name: CT.cellName CT.API
    , glyph: CT.cellGlyph CT.API
    , component: parentComponent render eval
    , initialState: installedState initialState
    , _State: NC._APIState
    , _Query: NC.makeQueryPrism NC._APIQuery
    }

render
  :: State
  -> APIHTML
render _ =
  H.slot unit \_ ->
    { component : FB.formBuilderComponent
    , initialState : installedState FB.initialState
    }

compileVarMap
  :: L.List Item.Model
  -> Port.VarMap
  -> Port.VarMap
compileVarMap fields globalVarMap =
  F.foldl alg SM.empty fields
  where
    alg =
      flip \{ name, fieldType, defaultValue } ->
        M.maybe id (SM.insert name) $
          SM.lookup name globalVarMap
            <|> (Item.defaultValueToVarMapValue fieldType =<< defaultValue)

eval :: Natural NC.CellEvalQuery APIDSL
eval q =
  case q of
    NC.EvalCell info k ->
      k <$> NC.runCellEvalT do
        fields <-
          query unit (request (FB.GetItems >>> left))
            # MT.lift
            >>= M.maybe (EC.throwError "Error querying FormBuilder") pure
        pure $ Port.VarMap $ compileVarMap fields info.globalVarMap
    NC.SetupCell _ next ->
      pure next
    NC.NotifyRunCell next ->
      pure next
    NC.Save k ->
      query unit (request (FB.GetItems >>> left)) <#>
        M.maybe [] L.fromList
          >>> { items : _ }
          >>> Model.encode
          >>> k
    NC.Load json next -> do
      F.for_ (Model.decode json) \{items} ->
        query unit $ action (FB.SetItems (L.toList items) >>> left)
      pure next
    NC.SetCanceler _ next -> pure next

getChildF :: forall i f. Natural (ChildF i f) f
getChildF (ChildF _ q) =
  q

queryShouldRun
  :: forall a
   . QueryP a
  -> Boolean
queryShouldRun =
  coproduct
    (const false)
    (getChildF >>>
       coproduct
         (const false)
         (getChildF >>> pred))

  where
    pred q =
      case q of
        Item.Update _ -> true
        _ -> false
