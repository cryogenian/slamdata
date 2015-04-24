-- | Resource tag for item
module Model.File.Resource where

import Data.Either (Either(..))
import Data.Foreign (ForeignError(TypeMismatch))
import Data.Foreign.Class (IsForeign, read)

data Resource = File | Database | Notebook | Directory | Table

instance eqResource :: Eq Resource where
  (==) File File = true
  (==) Database Database = true
  (==) Notebook Notebook = true
  (==) Directory Directory = true
  (==) Table Table = true
  (==) _ _ = false
  (/=) a b = not $ a == b

resource2str :: Resource -> String
resource2str r = case r of
  File -> "file"
  Database -> "mount"
  Notebook -> "notebook"
  Directory -> "directory"
  Table -> "table"

-- | Now only `IsForeign`. After switching to `purescript-affjax`
-- | will be `EncodeJson`
instance resourceIsForeign :: IsForeign Resource where
  read f = do
    str <- read f
    case str of
      "file" -> pure File
      "mount" -> pure Database
      "notebook" -> pure Notebook
      "directory" -> pure Directory
      "table" -> pure Table
      _ -> Left $ TypeMismatch "resource" "string"

