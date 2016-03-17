module Test.SlamData.Feature.Effects where

import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Random (RANDOM())
import Control.Monad.Eff.Ref (REF())
import DOM (DOM())
import Database.Mongo.Mongo (DB())
import Graphics.EasyImage (EASY_IMAGE())
import Graphics.ImageDiff (IMAGE_MAGICK())
import Node.Buffer (BUFFER())
import Node.ChildProcess (CHILD_PROCESS())
import Node.FS (FS())
import Node.Process (PROCESS())
import Platform (PLATFORM())
import Selenium.Types (SELENIUM())
import Test.SlamData.Feature.Env (ENV())

type SlamFeatureEffects e =
  ( childProcess :: CHILD_PROCESS
  , fs :: FS
  , avar :: AVAR
  , process :: PROCESS
  , db :: DB
  , buffer :: BUFFER
  , random :: RANDOM
  , env :: ENV
  | e)
