
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

module SlamData.Quasar.Auth.IdToken where

import SlamData.Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Apply as Apply
import Control.Bind ((=<<))
import Control.Monad.Aff.Free (class Affable, fromEff)
import Control.Monad.Eff (Eff)
import Control.UI.Browser as Browser

import Data.Array as A
import Data.Either as E
import Data.Maybe as M
import Data.Foldable as F
import Data.Traversable as T

import DOM (DOM)
import DOM.HTML as DOMHTML

import Network.HTTP.RequestHeader (RequestHeader)

import OIDC.Crypt.Types as OIDCT
import OIDC.Crypt.JSONWebKey (JSONWebKey)
import OIDC.Crypt as OIDC
import OIDC.Aff as OIDCAff

import SlamData.Config as Config
import SlamData.Quasar.Auth.Permission as P

import Quasar.Advanced.QuasarAF.Interpreter.Affjax (authHeader, permissionsHeader)
import Quasar.Advanced.Types as QAT

import Utils.LocalStorage as LS
import Utils.DOM as DOMUtils

