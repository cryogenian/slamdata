module Entries.Common where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff())
import Data.Foreign.Class (readProp)
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import DOM (DOM())
import Network.HTTP.Affjax (AJAX(), get)
import Utils (setDocumentTitle)

import qualified Data.DOM.Simple.Window as W

setSlamDataTitle :: forall eff. Maybe String -> Eff (dom :: DOM | eff) Unit
setSlamDataTitle maybeVersion = do
  let version = maybe "" (" " ++) maybeVersion
  setDocumentTitle $ "SlamData" <> version

getVersion :: forall eff. Aff (ajax :: AJAX | eff) (Maybe String)
getVersion = do
  serverInfo <- get Config.serverInfoUrl
  return $ either (const Nothing) Just (readProp "version" serverInfo.response)
