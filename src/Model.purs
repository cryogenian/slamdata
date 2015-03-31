-- | Input, output messages and state
module Model where

import  Control.Timer (Timeout())
import Data.Maybe
import Data.Foreign
import Data.Foreign.Class
import Data.Either
import DOM
import qualified Data.String.Regex as Rgx

-- | Input messages 
data Input
  = Sorting Sort
  | BreadcrumbUpdate [Breadcrumb]
  | ItemsUpdate [Item]
  | ItemHover Number Boolean
  | ItemSelect Number Boolean
  | ItemAdd Item
  | SearchValidation Boolean
  | SearchSet String
  | SearchTimeout Timeout
  | SearchNextValue String
  | SetPath String
  | Resort
  | Remove Item

-- | Request Messages 
data Request
  = GoToRoute String
  | SetSort Sort
  | SearchChange (Maybe Timeout) String
  | Breadcrumb Breadcrumb
  | SearchSubmit Search
  | Open Item
  | Delete Item
  | Share Item
  | Move Item
  | Configure Item
  | CreateNotebook State
  | MountDatabase State
  | CreateFolder State
  | UploadFile Node State
  | FileListChanged Node State

-- | Sort direction
data Sort = Asc | Desc

-- | revese sort
notSort :: Sort -> Sort
notSort Asc = Desc
notSort _ = Asc

sort2string :: Sort -> String
sort2string Asc = "asc"
sort2string Desc = "desc"

string2sort :: String -> Maybe Sort
string2sort "asc" = Just Asc
string2sort "desc" = Just Desc
string2sort _ = Nothing

instance eqSort :: Eq Sort where
  (==) Asc Asc = true
  (==) Desc Desc = true
  (==) _ _ = false
  (/=) a b = not $ a == b

-- | Resource tag for item 
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

-- | Item in list
type Item = {
  selected :: Boolean,
  hovered :: Boolean,
  name :: String,
  resource :: Resource,
  root :: String,
  phantom :: Boolean
  }

-- | Will be `DecodeJson` instance or `Json -> Maybe Item`
-- | after switching to `affjax`
readItem :: Foreign -> F Item
readItem f = do
  name <- readProp "name" f
  resource <- readProp "type" f
  pure $
    if isNotebook name && resource == File then
      {
        selected: false,
        hovered: false,
        name: name,
        resource: Notebook,
        root: "",
        phantom: false
      }
      else
       {
         selected: false,
         hovered: false,
         name: name,
         resource: resource,
         root: "",
         phantom: false
       }
  where isNotebook name = name /=
                          Rgx.replace (Rgx.regex "\\.nb$" Rgx.noFlags) "" name

-- | link to upper directory
up :: String
up = ".."

-- | default item
initItem :: Item
initItem = {
  resource: Directory,
  hovered: false,
  selected: false,
  root: "",
  name: "",
  phantom: false
  }

-- | upper directory
upLink :: Item
upLink = initItem{name = up}

-- | new directory conf
initDirectory :: Item
initDirectory = initItem{phantom = true}

-- | new notebook item conf
initNotebook :: Item
initNotebook = initItem{resource = Notebook}

-- | new file item conf
initFile :: Item
initFile = initItem{resource = File}

-- | sorting item list with preserving `upItem` in top
sortItem :: Sort -> Item -> Item -> Ordering
sortItem dir a b =
 let project = _.name
 in if project a == up then LT
    else if project b == up then GT
         else case dir of
           Asc -> compare (project a) (project b)
           Desc -> compare (project b) (project a)

-- | Model for one breadcrumb 
type Breadcrumb = {
  link :: String,
  name :: String
  }

rootBreadcrumb :: Breadcrumb
rootBreadcrumb = {
  name: "root",
  link: "/"
  }

-- | State of search field
type Search = {
  valid :: Boolean,
  value :: String,
  -- if _value_ has been changed but path hasn't been setted
  timeout :: Maybe Timeout,
  -- value to set path
  nextValue :: String
  }

initialSearch :: Search
initialSearch = {
  valid : true,
  value : "",
  timeout : Nothing,
  nextValue: ""
  }
-- | Application state
type State = {
  search :: Search,
  sort :: Sort,
  items :: [Item],
  breadcrumbs :: [Breadcrumb],
  path :: String
  }

initialState :: State
initialState = {
  search : initialSearch,
  sort : Asc,
  items : [],
  breadcrumbs : [],
  path: ""
  }

-- | Notebook cell type
data CellType
  = Evaluate
  | Explore
  | Search
  | Query
  | Visualize
  | Markdown

type CellMetadata = {}

-- | Cell model
type Cell = {
  input :: String,
  output :: String,
  cellType :: CellType,
  metadata :: CellMetadata
  }

type NbMetadata = {}

type Notebook = {
  metadata :: NbMetadata,
  cells :: [Cell]
  }

newNotebook :: Notebook
newNotebook = {
  metadata: {},
  cells: []
  }
  
