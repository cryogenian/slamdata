module Notebook.Cell.RunState (RunState(..), isRunning) where

import Data.Time (Milliseconds())

data RunState
  = RunInitial
  | RunElapsed Milliseconds
  | RunFinished Milliseconds

isRunning :: RunState -> Boolean
isRunning (RunElapsed _) = true
isRunning _ = false
