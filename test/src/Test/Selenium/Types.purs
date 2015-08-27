{-
Copyright 2015 SlamData, Inc.

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

module Test.Selenium.Types where

import Prelude

newtype EnabledRecord =
  EnabledRecord { ff :: Boolean
                , sf :: Boolean
                , fb :: Boolean
                , sb :: Boolean
                , value :: String 
                }

runEnabledRecord :: EnabledRecord -> { ff :: Boolean
                                     , sf :: Boolean
                                     , fb :: Boolean
                                     , sb :: Boolean
                                     , value :: String
                                     }
runEnabledRecord (EnabledRecord r) = r

instance eqEnabledRecord :: Eq EnabledRecord where
  eq (EnabledRecord r) (EnabledRecord r') =
    r.ff == r'.ff &&
    r.sf == r'.sf &&
    r.fb == r'.fb &&
    r.sb == r'.sb &&
    r.value == r'.value 

instance showEnabledRecord :: Show EnabledRecord where
  show (EnabledRecord r) =
    "(EnabledRecord { ff = " <> show r.ff <>
    ", sf = " <> show r.sf <>
    ", fb = " <> show r.fb <>
    ", sb = " <> show r.sb <>
    ", value = " <> show r.value <> 
    "})"

    
type RowCount = {table :: Int, pager :: Int}
