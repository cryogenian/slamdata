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

module SlamData.Workspace.MillerColumns.Column.Component.Request where

import SlamData.Prelude

import Data.List as L
import Data.Newtype (over)

-- | An ID used to annotate a load request or response that is specific to a
-- | particular column.
newtype RequestId = RequestId Int

derive instance newtypeRequestId ∷ Newtype RequestId _
derive newtype instance eqRequestId ∷ Eq RequestId
derive newtype instance ordRequestId ∷ Ord RequestId

succ ∷ RequestId → RequestId
succ = over RequestId (_ + 1)

type LoadRequest =
  { requestId ∷ RequestId
  , filter ∷ String
  , offset ∷ Maybe Int
  }

type LoadResponse a =
  { requestId ∷ RequestId
  , items ∷ L.List a
  , nextOffset ∷ Maybe Int
  }
