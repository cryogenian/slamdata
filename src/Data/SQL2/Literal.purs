{-
Copyright 2015 SlamLiteral, Inc.

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

module Data.SQL2.Literal
  ( module Data.SQL2.Literal.Core
  , module Data.SQL2.Literal.Parse
  ) where

import Data.SQL2.Literal.Core (Literal, LiteralF(Array, Boolean, Date, DateTime, Decimal, Integer, Interval, Null, Object, ObjectId, OrderedSet, String, Time), array, boolean, date, dateTime, decimal, integer, interval, null, object, objectId, orderedSet, renderLiteral, renderLiteralF, string, time)
import Data.SQL2.Literal.Parse (parseLiteral, parseLiteralF)
