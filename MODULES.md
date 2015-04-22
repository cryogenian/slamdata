# Module Documentation

## Module Config

#### `uploadUrl`

``` purescript
uploadUrl :: String
```


#### `metadataUrl`

``` purescript
metadataUrl :: String
```


#### `dataUrl`

``` purescript
dataUrl :: String
```


#### `notebookUrl`

``` purescript
notebookUrl :: String
```


#### `searchTimeout`

``` purescript
searchTimeout :: Number
```


#### `slamDataHome`

``` purescript
slamDataHome :: String
```


#### `userEnabled`

``` purescript
userEnabled :: Boolean
```


#### `newFolderName`

``` purescript
newFolderName :: String
```


#### `notebookExtension`

``` purescript
notebookExtension :: String
```


#### `newNotebookName`

``` purescript
newNotebookName :: String
```



## Module EffectTypes

#### `FileComponentEff`

``` purescript
type FileComponentEff e = (random :: Random, zClipboard :: Z.ZCLIPBOARD, file :: Uf.ReadFile, avar :: A.AVAR, ajax :: Af.AJAX, timer :: Tm.Timer | e)
```


#### `FileAppEff`

``` purescript
type FileAppEff e = Hl.HalogenEffects (FileComponentEff e)
```



## Module Utils


Shortcuts and glue between different modules

#### `encodeURIComponent`

``` purescript
encodeURIComponent :: String -> String
```

#### `decodeURIComponent`

``` purescript
decodeURIComponent :: String -> String
```


#### `log`

``` purescript
log :: forall a e. a -> Eff (trace :: Trace | e) Unit
```


#### `onLoad`

``` purescript
onLoad :: forall e. Eff (dom :: DOM | e) Unit -> Eff (dom :: DOM | e) Unit
```


#### `append`

``` purescript
append :: forall e. HTMLElement -> HTMLElement -> Eff (dom :: DOM | e) HTMLElement
```


#### `newTab`

``` purescript
newTab :: forall e. String -> Eff (dom :: DOM | e) Unit
```

Opens url in new tab or window

#### `convertToElement`

``` purescript
convertToElement :: Node -> HTMLElement
```

converts `Node` to `HTMLElement`

#### `reload`

``` purescript
reload :: forall e. Eff (dom :: DOM | e) Unit
```


#### `clearValue`

``` purescript
clearValue :: forall e. Node -> Eff (dom :: DOM | e) Unit
```


#### `select`

``` purescript
select :: forall e. Node -> Eff (dom :: DOM | e) Unit
```


#### `selectAff`

``` purescript
selectAff :: forall e. Node -> Aff (dom :: DOM | e) Unit
```


#### `locationOrigin`

``` purescript
locationOrigin :: forall e. DOMLocation -> Eff (dom :: DOM | e) String
```


#### `locationString`

``` purescript
locationString :: forall e. Eff (dom :: DOM | e) String
```


#### `trimQuotes`

``` purescript
trimQuotes :: String -> String
```



## Module Api.Fs


This module will be reworked after `affjax` is ready.

#### `listingIsForeign`

``` purescript
instance listingIsForeign :: IsForeign Listing
```


#### `childIsForeign`

``` purescript
instance childIsForeign :: IsForeign Child
```


#### `listingResponsable`

``` purescript
instance listingResponsable :: Ar.Responsable Listing
```


#### `listing`

``` purescript
listing :: forall e. String -> Aff.Aff (ajax :: Af.AJAX | e) [Mi.Item]
```


#### `makeFile`

``` purescript
makeFile :: forall e. Mi.Item -> String -> Aff.Aff (ajax :: Af.AJAX | e) Unit
```


#### `makeNotebook`

``` purescript
makeNotebook :: forall e. Mi.Item -> Mn.Notebook -> Aff.Aff (ajax :: Af.AJAX | e) Unit
```


#### `deleteItem`

``` purescript
deleteItem :: forall e. Mi.Item -> Aff.Aff (ajax :: Af.AJAX | e) Unit
```


#### `moveItem`

``` purescript
moveItem :: forall e. Mi.Item -> String -> Aff.Aff (ajax :: Af.AJAX | e) String
```



## Module App.File

#### `app`

``` purescript
app :: forall p e. Component p (Event (FileAppEff e)) Input Input
```



## Module App.Notebook

#### `app`

``` purescript
app :: forall p m. (Applicative m) => Component p m Input Input
```



## Module Controller.File


File component main handler 

#### `handler`

``` purescript
handler :: forall e. M.Request -> E.Event (FileAppEff e) M.Input
```


#### `getDirectories`

``` purescript
getDirectories :: forall e. String -> E.Event (FileAppEff e) M.Input
```


#### `selectThis`

``` purescript
selectThis :: forall e o. Et.Event o -> E.EventHandler (E.Event (dom :: DOM | e) M.Input)
```


#### `rename`

``` purescript
rename :: forall e. Mi.Item -> String -> E.EventHandler (E.Event (FileAppEff e) M.Input)
```


#### `checkRename`

``` purescript
checkRename :: forall e. String -> M.RenameDialogRec -> E.EventHandler (E.Event (FileAppEff e) M.Input)
```


#### `renameItemClicked`

``` purescript
renameItemClicked :: forall e. String -> String -> E.EventHandler (E.Event (FileAppEff e) M.Input)
```


## Module Driver.File


Module handles outer messages to `halogen` application
Mostly consists of routing functions 

#### `outside`

``` purescript
outside :: forall e. Hl.Driver M.Input (FileComponentEff e) -> Eff (FileAppEff e) Unit
```

Entry, used in `halogen` app

#### `searchPath`

``` purescript
searchPath :: S.SearchQuery -> Maybe String
```

Extract path predicate from search query

#### `updateSort`

``` purescript
updateSort :: Ms.Sort -> String -> String
```


#### `updateQ`

``` purescript
updateQ :: String -> String -> String
```


#### `updateSalt`

``` purescript
updateSalt :: String -> String -> String
```


#### `setSort`

``` purescript
setSort :: Ms.Sort -> String
```


#### `getPath`

``` purescript
getPath :: String -> String
```

extract path from full hash

#### `updatePath`

``` purescript
updatePath :: String -> String -> String
```

set _path_ value in path-labeled predicate in route


## Module Driver.Notebook

#### `Routes`

``` purescript
data Routes
  = Cell M.Path M.CellId Resume
  | Notebook M.Path Resume
```


#### `routing`

``` purescript
routing :: R.Match Routes
```


#### `driver`

``` purescript
driver :: forall e. H.Driver M.Input e -> Eff (H.HalogenEffects e) Unit
```


## Module Entries.File

#### `main`

``` purescript
main :: Eff (FileAppEff ()) Unit
```



## Module Entries.Notebook


## Module Input.File


file component state update function 

#### `inner`

``` purescript
inner :: M.State -> M.Input -> M.State
```



## Module Input.Notebook

#### `updateState`

``` purescript
updateState :: M.State -> M.Input -> M.State
```



## Module Model.Breadcrumb

#### `Breadcrumb`

``` purescript
type Breadcrumb = { name :: String, link :: String }
```

Model for one breadcrumb 

#### `rootBreadcrumb`

``` purescript
rootBreadcrumb :: Breadcrumb
```



## Module Model.File


Input, output messages and state for file component

#### `Input`

``` purescript
data Input
  = Sorting Sort
  | ItemsUpdate [Item] Sort
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
  | Loading Boolean
  | Focus Boolean
  | SetSearching Boolean
  | SetDialog (Maybe DialogResume)
  | AddRenameDirs [String]
  | SetRenameSelected String
  | RenameChanged String
  | RenameError String
  | RenameIncorrect Boolean
  | RenameSelectedContent [String]
```

Input messages 

#### `Request`

``` purescript
data Request
  = GoToRoute String
  | SetSort Sort
  | SearchChange Search String String
  | SearchClear Boolean Search
  | Breadcrumb Breadcrumb
  | SearchSubmit Search String
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
```

Request Messages 

#### `RenameDialogRec`

``` purescript
type RenameDialogRec = { selectedContent :: [String], incorrect :: Boolean, error :: String, target :: String, selected :: String, dirs :: [String], item :: Item, showList :: Boolean }
```


#### `initialRenameDialog`

``` purescript
initialRenameDialog :: Item -> RenameDialogRec
```


#### `DialogResume`

``` purescript
data DialogResume
  = RenameDialog RenameDialogRec
  | ConfigureDialog 
  | MountDialog 
  | ShareDialog String
```


#### `eqDialogResume`

``` purescript
instance eqDialogResume :: Eq DialogResume
```


#### `State`

``` purescript
type State = { dialog :: Maybe DialogResume, searching :: Boolean, path :: String, breadcrumbs :: [Breadcrumb], items :: [Item], sort :: Sort, search :: Search }
```

Application state

#### `initialState`

``` purescript
initialState :: State
```



## Module Model.Item

#### `Item`

``` purescript
type Item = { phantom :: Boolean, root :: String, resource :: Resource, name :: String, hovered :: Boolean, selected :: Boolean }
```

Item in list

#### `itemPath`

``` purescript
itemPath :: Item -> String
```


#### `up`

``` purescript
up :: String
```

link to upper directory

#### `initItem`

``` purescript
initItem :: Item
```

default item

#### `upLink`

``` purescript
upLink :: Item
```

upper directory 

#### `initDirectory`

``` purescript
initDirectory :: Item
```

new directory conf

#### `initNotebook`

``` purescript
initNotebook :: Item
```

new notebook item conf

#### `initFile`

``` purescript
initFile :: Item
```

new file item conf

#### `sortItem`

``` purescript
sortItem :: Boolean -> Sort -> Item -> Item -> Ordering
```

sorting item list with preserving `upItem` in top
If first argument is true then sorting not only by name
but by `item.root <> item.name`


## Module Model.Notebook

#### `Input`

``` purescript
data Input
  = ViewNotebook String (List CellState)
  | EditNotebook String (List CellState)
  | ViewCell CellState
  | EditCell CellState
```


#### `Request`

``` purescript
data Request
  = Request 
```


#### `State`

``` purescript
data State
  = OneCellView Resume CellState
  | NotebookView Resume String (List CellState)
```


#### `CellState`

``` purescript
type CellState = { id :: String }
```


#### `initialCell`

``` purescript
initialCell :: CellState
```


#### `initialState`

``` purescript
initialState :: State
```


#### `CellId`

``` purescript
type CellId = String
```


#### `string2cellId`

``` purescript
string2cellId :: String -> Either String CellId
```


#### `CellType`

``` purescript
data CellType
  = Evaluate 
  | Explore 
  | Search 
  | Query 
  | Visualize 
  | Markdown 
```


#### `Cell`

``` purescript
newtype Cell
  = Cell { metadata :: String, cellType :: CellType, output :: String, input :: String }
```


#### `Notebook`

``` purescript
newtype Notebook
  = Notebook { cells :: [Cell], metadata :: String }
```


#### `newNotebook`

``` purescript
newNotebook :: Notebook
```


#### `cellTypeEncode`

``` purescript
instance cellTypeEncode :: Ae.EncodeJson CellType
```


#### `cellEncode`

``` purescript
instance cellEncode :: Ae.EncodeJson Cell
```


#### `notebookEncode`

``` purescript
instance notebookEncode :: Ae.EncodeJson Notebook
```


#### `notebookRequestable`

``` purescript
instance notebookRequestable :: Ar.Requestable Notebook
```



## Module Model.Path

#### `Path`

``` purescript
data Path
  = Path (List String) String
```


#### `path2str`

``` purescript
path2str :: Path -> String
```


#### `decodeURIPath`

``` purescript
decodeURIPath :: String -> String
```


#### `encodeURIPath`

``` purescript
encodeURIPath :: String -> String
```


#### `hidePath`

``` purescript
hidePath :: String -> String -> String
```


#### `cleanPath`

``` purescript
cleanPath :: String -> String
```



## Module Model.Resource


Resource tag for item 

#### `Resource`

``` purescript
data Resource
  = File 
  | Database 
  | Notebook 
  | Directory 
  | Table 
```


#### `eqResource`

``` purescript
instance eqResource :: Eq Resource
```


#### `resource2str`

``` purescript
resource2str :: Resource -> String
```


#### `resourceIsForeign`

``` purescript
instance resourceIsForeign :: IsForeign Resource
```

Now only `IsForeign`. After switching to `purescript-affjax`
will be `EncodeJson`


## Module Model.Resume

#### `Resume`

``` purescript
data Resume
  = View 
  | Edit 
```


#### `string2resume`

``` purescript
string2resume :: String -> Either String Resume
```


#### `resumeEq`

``` purescript
instance resumeEq :: Eq Resume
```



## Module Model.Search

#### `Search`

``` purescript
type Search = { loading :: Boolean, nextValue :: String, timeout :: Maybe Timeout, value :: String, focused :: Boolean, valid :: Boolean }
```

State of search field

#### `initialSearch`

``` purescript
initialSearch :: Search
```



## Module Model.Sort


Sort direction

#### `Sort`

``` purescript
data Sort
  = Asc 
  | Desc 
```


#### `notSort`

``` purescript
notSort :: Sort -> Sort
```

revese sort

#### `sort2string`

``` purescript
sort2string :: Sort -> String
```


#### `string2sort`

``` purescript
string2sort :: String -> Either String Sort
```


#### `eqSort`

``` purescript
instance eqSort :: Eq Sort
```



## Module Utils.Event

#### `raiseEvent`

``` purescript
raiseEvent :: forall e a. String -> HTMLElement -> Eff (dom :: DOM | e) HTMLElement
```



## Module Utils.File

#### `FileReader`

``` purescript
data FileReader :: *
```


#### `File`

``` purescript
data File :: *
```


#### `FileList`

``` purescript
data FileList :: *
```


#### `ReadFile`

``` purescript
data ReadFile :: !
```


#### `fileListToArray`

``` purescript
fileListToArray :: FileList -> [File]
```


#### `name`

``` purescript
name :: forall e. File -> Eff (file :: ReadFile | e) String
```


#### `newReaderEff`

``` purescript
newReaderEff :: forall e. Eff e FileReader
```

#### `newReader`

``` purescript
newReader :: forall e. Aff e FileReader
```


#### `readAsBinaryStringEff`

``` purescript
readAsBinaryStringEff :: forall e. File -> FileReader -> Eff (file :: ReadFile | e) Unit
```


#### `files`

``` purescript
files :: forall e. Node -> Aff (dom :: DOM | e) FileList
```


#### `readAsBinaryString`

``` purescript
readAsBinaryString :: forall e. File -> FileReader -> Aff (file :: ReadFile | e) String
```



## Module Utils.Halide

#### `targetLink`

``` purescript
targetLink :: forall i m. (Alternative m) => i -> [A.Attr (m i)]
```


#### `targetLink'`

``` purescript
targetLink' :: forall i m. (Alternative m) => m i -> [A.Attr (m i)]
```


#### `readonly`

``` purescript
readonly :: forall i. Boolean -> A.Attr i
```



## Module View.Css

#### `searchInput`

``` purescript
searchInput :: ClassName
```


#### `searchClear`

``` purescript
searchClear :: ClassName
```


#### `searchPath`

``` purescript
searchPath :: ClassName
```


#### `searchPathActive`

``` purescript
searchPathActive :: ClassName
```


#### `searchAffix`

``` purescript
searchAffix :: ClassName
```


#### `searchPathBody`

``` purescript
searchPathBody :: ClassName
```


#### `searchAffixEmpty`

``` purescript
searchAffixEmpty :: ClassName
```


#### `results`

``` purescript
results :: ClassName
```


#### `logo`

``` purescript
logo :: ClassName
```


#### `navCont`

``` purescript
navCont :: ClassName
```


#### `navIcon`

``` purescript
navIcon :: ClassName
```


#### `navLogo`

``` purescript
navLogo :: ClassName
```


#### `search`

``` purescript
search :: ClassName
```


#### `toolbarSort`

``` purescript
toolbarSort :: ClassName
```


#### `toolbarMenu`

``` purescript
toolbarMenu :: ClassName
```


#### `itemIcon`

``` purescript
itemIcon :: ClassName
```


#### `itemToolbar`

``` purescript
itemToolbar :: ClassName
```


#### `itemContent`

``` purescript
itemContent :: ClassName
```


#### `directoryListGroup`

``` purescript
directoryListGroup :: ClassName
```



## Module View.File

#### `view`

``` purescript
view :: forall p e. (Request -> E.Event _ Input) -> State -> H.HTML p (E.Event _ Input)
```



## Module View.Notebook

#### `view`

``` purescript
view :: forall p i. State -> H.HTML p i
```



## Module View.File.Modal

#### `modal`

``` purescript
modal :: forall p e. State -> H.HTML p (E.Event (FileAppEff e) Input)
```



## Module View.File.Modal.Common

#### `h4`

``` purescript
h4 :: forall p i. String -> [H.HTML p i]
```


#### `section`

``` purescript
section :: forall p i. [A.ClassName] -> [H.HTML p i] -> H.HTML p i
```


#### `header`

``` purescript
header :: forall p i. [H.HTML p i] -> H.HTML p i
```


#### `body`

``` purescript
body :: forall p i. [H.HTML p i] -> H.HTML p i
```


#### `footer`

``` purescript
footer :: forall p i. [H.HTML p i] -> H.HTML p i
```



## Module View.File.Modal.RenameDialog

#### `renameDialog`

``` purescript
renameDialog :: forall p e. RenameDialogRec -> [H.HTML p (E.Event (FileAppEff e) Input)]
```



## Module View.File.Modal.ShareDialog

#### `shareDialog`

``` purescript
shareDialog :: forall p e. String -> [H.HTML p (E.Event (FileAppEff e) Input)]
```




