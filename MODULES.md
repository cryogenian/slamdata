# Module Documentation

## Module App

#### `app`

``` purescript
app :: forall e. Hl.UI M.Input Void M.Request (ajax :: Af.Ajax, file :: Uf.ReadFile, timer :: Tm.Timer | e)
```



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



## Module Main


## Module Model


Input, output messages and state

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
```

Input messages 

#### `Request`

``` purescript
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
```

Request Messages 

#### `State`

``` purescript
type State = { path :: String, breadcrumbs :: [Breadcrumb], items :: [Item], sort :: Sort, search :: Search }
```

Application state

#### `initialState`

``` purescript
initialState :: State
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



## Module View

#### `view`

``` purescript
view :: forall u node. (H.HTMLRepr node) => M.State -> node u (Either M.Input M.Request)
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
listing :: forall e. String -> Aff.Aff (ajax :: Af.Ajax | e) [Mi.Item]
```


#### `makeFile`

``` purescript
makeFile :: forall e. String -> String -> Af.Affjax e Unit
```


#### `makeNotebook`

``` purescript
makeNotebook :: forall e. Af.URL -> Mn.Notebook -> Af.Affjax e Unit
```


#### `deleteItem`

``` purescript
deleteItem :: forall e. Mi.Item -> Af.Affjax e Unit
```



## Module Controller.Driver


Module handles outer messages to `halogen` application
Mostly consists of routing functions 

#### `outside`

``` purescript
outside :: forall e. Hl.Driver M.Input (ajax :: Af.Ajax, timer :: Tm.Timer | e) -> Eff (Hl.HalogenEffects (ajax :: Af.Ajax, timer :: Tm.Timer | e)) Unit
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

#### `addToPath`

``` purescript
addToPath :: String -> String -> String
```

Add to _path_ value in path-labeled predicate 


## Module Controller.Input


state update function 

#### `inner`

``` purescript
inner :: M.State -> M.Input -> M.State
```



## Module Controller.Request


Main app handler

#### `handler`

``` purescript
handler :: forall e. M.Request -> Aff.Aff (Hl.HalogenEffects (ajax :: Af.Ajax, file :: Uf.ReadFile, timer :: Tm.Timer | e)) M.Input
```



## Module Model.Item

#### `Item`

``` purescript
type Item = { phantom :: Boolean, root :: String, resource :: Resource, name :: String, hovered :: Boolean, selected :: Boolean }
```

Item in list

#### `readItem`

``` purescript
readItem :: Foreign -> F Item
```

Will be `DecodeJson` instance or `Json -> Maybe Item`
after switching to `affjax`

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
sortItem :: Sort -> Item -> Item -> Ordering
```

sorting item list with preserving `upItem` in top

#### `Breadcrumb`

``` purescript
type Breadcrumb = { name :: String, link :: String }
```

Model for one breadcrumb 

#### `rootBreadcrumb`

``` purescript
rootBreadcrumb :: Breadcrumb
```


#### `Search`

``` purescript
type Search = { nextValue :: String, timeout :: Maybe Timeout, value :: String, valid :: Boolean }
```

State of search field

#### `initialSearch`

``` purescript
initialSearch :: Search
```



## Module Model.Notebook

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


#### `resultImpl`

``` purescript
resultImpl :: forall e a. Fn3 (Maybe a) (a -> Maybe a) FileReader (Eff (file :: ReadFile | e) (Maybe String))
```


#### `resultEff`

``` purescript
resultEff :: forall e. FileReader -> Eff (file :: ReadFile | e) (Maybe String)
```


#### `filesEff`

``` purescript
filesEff :: forall e. Node -> Eff (dom :: DOM | e) FileList
```

#### `files`

``` purescript
files :: forall e. Node -> Aff (dom :: DOM | e) FileList
```


#### `onloadEff`

``` purescript
onloadEff :: forall e e'. FileReader -> Eff e Unit -> Eff (file :: ReadFile | e') Unit
```


#### `readAsBinaryString`

``` purescript
readAsBinaryString :: forall e. File -> FileReader -> Aff (file :: ReadFile | e) String
```



## Module Utils.Halide

#### `back`

``` purescript
back :: forall e i r. i -> EventHandler (Either i r)
```


#### `request`

``` purescript
request :: forall e i r. r -> EventHandler (Either i r)
```


#### `targetLink`

``` purescript
targetLink :: forall a b. b -> [A.Attr (Either a b)]
```




