module Entries.Common where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff())
import Data.Foreign.Class (readProp)
import Data.Either (either)
import DOM (DOM())
import Network.HTTP.Affjax (AJAX(), get)
import Utils (setDocumentTitle)

import qualified Data.DOM.Simple.Window as W

setSlamDataTitle :: forall eff. Aff (ajax :: AJAX, dom :: DOM | eff) Unit
setSlamDataTitle = do
  serverInfo <- get Config.serverInfoUrl
  let version = either (const "") (" " ++) (readProp "version" serverInfo.response)
  liftEff $ setDocumentTitle $ "SlamData" <> version
  return unit
