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

module Dialog.Mount
  ( comp
  , module Dialog.Mount.Query
  , module Dialog.Mount.State
  ) where

import Prelude

import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Eff.Exception (message, Error())
import Control.Monad.Eff.Ref (REF())
import Control.UI.Browser (clearValue, select)
import Data.Array (null, filter)
import Data.Date (Now())
import Data.Either (either, Either(..))
import Data.Foldable (any)
import Data.Foreign (parseJSON)
import Data.Foreign.Class (readProp)
import Data.Lens ((^.), (.~), (?~))
import Data.Maybe (Maybe(..), isNothing, isJust)
import Data.Path.Pathy ((</>), dir)
import Data.String.Regex as Rx
import Data.URI (runParseAbsoluteURI, printAbsoluteURI)
import Dialog.Mount.Query
import Dialog.Mount.Render
import Dialog.Mount.State
import Halogen
import Model.Resource as R
import Network.HTTP.Affjax (AJAX())
import Quasar.Aff as API
import Utils.URI (toURI)

type Slam e = Aff (HalogenEffects ( ajax :: AJAX
                                  , now :: Now
                                  , ref :: REF
                                  | e) )

comp :: forall e. Component State Query (Slam e)
comp = component render eval

eval :: forall e. Eval Query State Query (Slam e)
eval (ClearValue el next) = do
  liftEff' $ clearValue el
  pure next
eval (SelectElement el next) = do
  liftEff' $ select el
  pure next
eval (UpdateConnectionURI uri next) = do
  state <- get
  case uri of
    "" -> modify (const $ initialState state.parent)
    _ -> modify fn
  pure next
  where
  fn state = either (const $ incorrectRecord state) (correctRecord state)
             $ runParseAbsoluteURI uri
  incorrectRecord state =
    state { connectionURI = uri
          , message = pure "Pasted value does not appear to be a valid connection URI"
          , valid = false
          }
  correctRecord state uri' =
    let hosts = hostsFromURI uri'
        validation = validate state hosts
    in state { connectionURI = printAbsoluteURI uri'
             , hosts = hosts <> [ initialMountHost ]
             , path = pathFromURI uri'
             , user = userFromURI uri'
             , password = passwordFromURI uri'
             , props = propsFromURI uri' <> [ initialMountProp ]
             , message = validation
             , valid = isNothing validation
             }

eval (Dismiss next) = do
  pure next
eval (Save next) = do
  state <- get
  let resource = R.Database $ state.parent </> dir state.name
  modify (_inProgress .~ true)
  modify (_externalValidationError .~ Nothing)
  result <- liftAff' $ attempt $ API.saveMount resource state.connectionURI
  case result of
    Left err -> do
      modify (_externalValidationError ?~ msg err)
    Right _ ->
      modify (_saved ?~ resource)
  pure next
  where
  msg :: Error -> String
  msg err = "There was a problem saving the mount: "
            <> extractErrorMessage (message err)

  extractErrorMessage :: String -> String
  extractErrorMessage msg' =
    case parseJSON msg' >>= readProp "error" of
      Left _ -> msg'
      Right msg'' -> msg''
eval (ModifyState fn next) = do
  modify composed
  pure next
  where
  composed d = d' { connectionURI = connectionURI
                  , hosts = if null hosts
                            then [ initialMountHost, initialMountHost ]
                            else hosts <> [ initialMountHost ]
                  , props = props <> [ initialMountProp ]
                  , message = validation
                  , valid = isNothing validation
                  }
    where
    d' = fn d
    hosts = filter (not <<< isEmptyHost) d'.hosts
    props = filter (not <<< isEmptyProp) d'.props
    connectionURI = mkURI d'.path d'.user d'.password hosts props
    validation = validate (fn d) hosts
    mkURI path user password hosts' props' =
      if any isValidHost hosts
      then toURI { path: nonEmpty path
                 , credentials: { user: _, password: _ }
                   <$> nonEmpty user
                   <*> nonEmpty password
                 , hosts: (\h -> h {port = nonEmpty h.port}) <$> hosts'
                 , props: props'
                 }
      else ""
    isValidHost {host: host} = isJust $ nonEmpty host

    nonEmpty :: String -> Maybe String
    nonEmpty s | Rx.test rxEmpty s = Nothing
    nonEmpty s = Just s

    rxEmpty :: Rx.Regex
    rxEmpty = Rx.regex "^\\s*$" Rx.noFlags

eval (GetSaved continue) = do
  res <- gets (^. _saved)
  pure $ continue res
