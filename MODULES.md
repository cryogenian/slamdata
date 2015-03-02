# Module Documentation

## Module Component


**action** - type of messages to components
it modifies **state** in function UpdateFn

#### `Receiver`

``` purescript
type Receiver message e = message -> Eff e Unit
```

One example of **Receiver** can be <code>send channel</code>

#### `UpdateFn`

``` purescript
type UpdateFn action state eff = action -> state -> Eff eff state
```

this function fold **state** with **action**

#### `RenderFn`

``` purescript
type RenderFn action state eff = Receiver action eff -> state -> Eff eff VTree
```

this function modifies virtual tree 

#### `Initial`

``` purescript
type Initial action state = { state :: state, action :: action }
```

This is just for reducing number of fields in spec 

#### `Widget`

``` purescript
type Widget action state eff = { insert :: HTMLElement -> Eff eff Unit, send :: Receiver action eff, signal :: Signal state }
```

This is basic component that can be rendered to DOM

#### `WidgetSpec`

``` purescript
type WidgetSpec action state eff = { hook :: Receiver action eff -> Eff eff Unit, initial :: Initial action state, updateState :: UpdateFn action state eff, render :: RenderFn action state eff }
```

Spec of component

#### `toVoid`

``` purescript
toVoid :: forall a e. Receiver a e
```

Shortcut to void receiver

#### `define`

``` purescript
define :: forall state action e. WidgetSpec action state (dom :: DOM, chan :: Chan | e) -> Eff (dom :: DOM, chan :: Chan | e) (Widget action state (dom :: DOM, chan :: Chan | e))
```

Takes **WidgetSpec** returns **Widget**


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



## Module Hash


Low level url-hash service

#### `Hash`

``` purescript
type Hash = String
```


#### `getHash`

``` purescript
getHash :: forall e. Eff e Hash
```


#### `setHash`

``` purescript
setHash :: forall e. String -> Eff e Unit
```


#### `changed`

``` purescript
changed :: forall e. Eff e Unit -> Eff e Unit
```



## Module Main



#### `main`

``` purescript
main :: Eff _ Unit
```



## Module Model


Common stuff to use in many components

#### `Sort`

``` purescript
data Sort
  = Asc 
  | Desc 
```


#### `sortToString`

``` purescript
sortToString :: Sort -> String
```


#### `sortFromString`

``` purescript
sortFromString :: String -> Maybe Sort
```


#### `Mount`

``` purescript
data Mount
  = File 
  | Database 
  | Notebook 
  | Directory 
  | Table 
```


#### `decodeJsonMount`

``` purescript
instance decodeJsonMount :: DecodeJson Mount
```



## Module Router


Router component. It works with only one route aggregate.

#### `State`

``` purescript
type State = { search :: String, sort :: Sort }
```

state of routing is sort direction and search string

#### `toHash`

``` purescript
toHash :: forall o. { search :: String, sort :: Sort | o } -> String
```

Make hash

#### `extractSort`

``` purescript
extractSort :: String -> Maybe Sort
```

Get sort from hash

#### `extractSearch`

``` purescript
extractSearch :: String -> String
```

Get search from hash

#### `fromHash`

``` purescript
fromHash :: String -> { search :: String, sort :: Sort }
```

Make state without hash-component from hash-string

#### `setSearch`

``` purescript
setSearch :: String -> Eff _ Unit
```

shortcut for setting search string
I think, that after some other services will be
developed pattern will arise

#### `getRoute`

``` purescript
getRoute :: forall e. Eff e State
```



## Module Utils


Shortcuts and glue between different modules

#### `log`

``` purescript
log :: forall a e. a -> Eff (trace :: Trace | e) Unit
```

It's simpler for me to use foreign logging
then add Show instances

#### `onLoad`

``` purescript
onLoad :: Eff _ Unit -> Eff _ Unit
```

Ok, I were was wrong, it's simplier to define onload
by simple-dom

#### `append`

``` purescript
append :: forall e. HTMLElement -> HTMLElement -> Eff (dom :: DOM | e) HTMLElement
```

append one element to another element
PR to simple-dom

#### `parentImpl`

``` purescript
parentImpl :: forall e a. Fn3 (Maybe a) (a -> Maybe a) HTMLElement (Eff e (Maybe HTMLElement))
```


#### `parent`

``` purescript
parent :: forall e. HTMLElement -> Eff e (Maybe HTMLElement)
```

get parent of element
PR to simple-dom

#### `hashChanged`

``` purescript
hashChanged :: forall a e. (String -> String -> Eff e Unit) -> Eff e Unit
```

need to PR to simple-dom

#### `convertToElement`

``` purescript
convertToElement :: Node -> HTMLElement
```

This function is needed to convert VTree -> Node -> HTMLElement 


## Module View


Application entry point

#### `State`

``` purescript
type State = { breadcrumb :: Breadcrumb.Output, toolbar :: Toolbar.State, list :: List.State, navbar :: Navbar.State }
```

State is multiplication of children state

#### `Action`

``` purescript
data Action
  = Init 
  | ListAction List.Action
  | NavbarAction Navbar.Action
  | ToolbarAction Toolbar.Action
  | BreadcrumbAction Breadcrumb.Input
```

Action is sum of children actions

#### `spec`

``` purescript
spec :: WidgetSpec Action State _
```

Spec 


## Module XHR


Low level service for managing xhr calls
Probably will be removed after moving to purescript-aff

#### `Input`

``` purescript
type Input a = { url :: String, additionalHeaders :: StrMap String, content :: A.HttpData a, method :: A.HttpMethod }
```


#### `Output`

``` purescript
type Output = { content :: String }
```


#### `run`

``` purescript
run :: forall a. Receiver Output _ -> Input a -> Eff _ Unit
```



## Module Api.Fs


Should be rewritten with purescript-aff

#### `Metadata`

``` purescript
newtype Metadata
  = Metadata { mount :: Mount, name :: String }
```

#### `MetadataResponse`

``` purescript
newtype MetadataResponse
  = MetadataResponse { children :: [Metadata] }
```


#### `decodeJsonMetadata`

``` purescript
instance decodeJsonMetadata :: DecodeJson Metadata
```


#### `decodeJsonMetadataResponse`

``` purescript
instance decodeJsonMetadataResponse :: DecodeJson MetadataResponse
```


#### `metadata`

``` purescript
metadata :: forall e. String -> ([Metadata] -> Eff _ Unit) -> Eff (dom :: DOM | e) Unit
```


#### `get`

``` purescript
get :: forall e. String -> Maybe Number -> Maybe Number -> ([Json] -> Eff _ Unit) -> Eff (dom :: DOM | e) Unit
```


#### `post`

``` purescript
post :: forall e. String -> Json -> ([Json] -> Eff _ Unit) -> Eff (dom :: DOM | e) Unit
```


#### `put`

``` purescript
put :: forall e. String -> Json -> (Boolean -> Eff _ Unit) -> Eff (dom :: DOM | e) Unit
```


#### `move`

``` purescript
move :: forall e. String -> String -> (Boolean -> Eff _ Unit) -> Eff (dom :: DOM | e) Unit
```



## Module Signal.Effectful



#### `foldpE`

``` purescript
foldpE :: forall a b c e. (a -> b -> Eff e b) -> b -> Signal a -> Eff e (Signal b)
```



## Module View.Back


This component will not be rendered alone, so, it has not a spec

#### `Action`

``` purescript
data Action
  = Init 
  | Clicked 
  | Changed State
  | Ajax XHR.Output
```


#### `State`

``` purescript
data State
  = Directory 
  | Database 
  | Table 
  | Notebook 
```


#### `viewIcon`

``` purescript
viewIcon :: State -> VTree
```


#### `view`

``` purescript
view :: Receiver Action _ -> State -> Eff _ VTree
```


#### `foldState`

``` purescript
foldState :: Action -> State -> Eff _ State
```


#### `hookFn`

``` purescript
hookFn :: forall e. Receiver Action (trace :: Trace, dom :: DOM, chan :: Chan | e) -> Eff (trace :: Trace, dom :: DOM, chan :: Chan | e) Unit
```



## Module View.Breadcrumb


Breadcrumb component is used for navigation up and bottom
in directory structure and to show where is user now
In this module I won't use terms <code>Action</code>
and <code>State</code>.

#### `Link`

``` purescript
type Link = { name :: String, href :: String }
```

Link that will be passed to search query in router

#### `Output`

``` purescript
type Output = { links :: [Link] }
```

Output signals: init breadcrumbs, set breadcrumbs links to
signal content

#### `emptyOut`

``` purescript
emptyOut :: Output
```


#### `Input`

``` purescript
data Input
  = Init 
  | GoTo Link
```

Input signal: go to search location specified in link

#### `renderLink`

``` purescript
renderLink :: Receiver Input _ -> Link -> VTree
```


#### `render`

``` purescript
render :: Receiver Input _ -> Output -> Eff _ VTree
```

Renders breadcrumb and send <code>GoTo</code> message
if clicked

#### `run`

``` purescript
run :: Input -> Output -> Eff _ Output
```

Transforming input signal to output signal 


## Module View.Item


This component won't be rendered alone, it hasn't spec

#### `Logic`

``` purescript
type Logic = { id :: String, name :: String, resource :: Mount }
```


#### `State`

``` purescript
type State = { isHovered :: Boolean, isSelected :: Boolean, logic :: Logic }
```


#### `Action`

``` purescript
data Action
  = Init 
  | Focus 
  | Blur 
  | Open 
  | Activate 
  | Unactivate 
  | Configure Logic
  | Trash Logic
  | Share Logic
```


#### `initialState`

``` purescript
initialState :: State
```


#### `renderResourceType`

``` purescript
renderResourceType :: Mount -> VTree
```


#### `renderMiniToolbar`

``` purescript
renderMiniToolbar :: Receiver Action _ -> State -> Eff _ [VTree]
```


#### `view`

``` purescript
view :: Receiver Action _ -> State -> Eff _ VTree
```


#### `foldState`

``` purescript
foldState :: Action -> State -> Eff _ State
```



## Module View.List


This component has no spec, it won't be rendered alone

#### `State`

``` purescript
type State = { items :: [Item.State] }
```

Its state has a list of children

#### `initialState`

``` purescript
initialState :: State
```


#### `Action`

``` purescript
data Action
  = Init 
  | ItemAction Number Item.Action
  | SortAction Sort
```

External messages will be marked with index of child
that send it

#### `view`

``` purescript
view :: Receiver Action _ -> State -> Eff _ VTree
```

Rendering of list

#### `foldState`

``` purescript
foldState :: Action -> State -> Eff _ State
```



## Module View.Logo


This component is not component :)
It is just two function and even have not action and state

#### `view`

``` purescript
view :: forall a b. a -> b -> Eff _ VTree
```

send and st will be removed


## Module View.Navbar


This component also has no spec, and will not be rendered alone

#### `State`

``` purescript
type State = { user :: User.State, back :: Back.State, search :: Search.State }
```

Multiplication of children state

#### `initialState`

``` purescript
initialState :: State
```


#### `Action`

``` purescript
data Action
  = Init 
  | SearchAction Search.Action
  | BackAction Back.Action
```

Sum of children actions

#### `view`

``` purescript
view :: Receiver Action _ -> State -> Eff _ VTree
```

Render

#### `foldState`

``` purescript
foldState :: Action -> State -> Eff _ State
```

Update state

#### `hookFn`

``` purescript
hookFn :: forall e. Receiver Action _ -> Eff _ Unit
```

listen route changes, called after render


## Module View.Search


Search component will not be rendered alone

#### `Action`

``` purescript
data Action
  = Init 
  | Change String
  | RouteChanged String
  | Submit 
```

Route change is external message

#### `State`

``` purescript
type State = { timeout :: Maybe Timeout, value :: String, valid :: Boolean }
```


#### `initialState`

``` purescript
initialState :: State
```


#### `view`

``` purescript
view :: Receiver Action _ -> State -> Eff _ VTree
```


#### `foldState`

``` purescript
foldState :: Action -> State -> Eff _ State
```

Update searcher state


## Module View.Shortcuts


Will be moved to VirtualDOM 

#### `div`

``` purescript
div :: forall props. {  | props } -> [VTree] -> VTree
```


#### `nav`

``` purescript
nav :: forall props. {  | props } -> [VTree] -> VTree
```


#### `a`

``` purescript
a :: forall props. {  | props } -> [VTree] -> VTree
```


#### `form`

``` purescript
form :: forall props. {  | props } -> [VTree] -> VTree
```


#### `input`

``` purescript
input :: forall props. {  | props } -> [VTree] -> VTree
```


#### `i`

``` purescript
i :: forall props. {  | props } -> [VTree] -> VTree
```


#### `span`

``` purescript
span :: forall props. {  | props } -> [VTree] -> VTree
```


#### `button`

``` purescript
button :: forall props. {  | props } -> [VTree] -> VTree
```


#### `ul`

``` purescript
ul :: forall props. {  | props } -> [VTree] -> VTree
```


#### `ol`

``` purescript
ol :: forall props. {  | props } -> [VTree] -> VTree
```


#### `li`

``` purescript
li :: forall props. {  | props } -> [VTree] -> VTree
```


#### `emptyVTree`

``` purescript
emptyVTree :: VTree
```

useful for defining of components

#### `jsVoid`

``` purescript
jsVoid :: String
```



## Module View.Toolbar


Mostly produce messages which must be consumed by external
i.e. upload file, or call Api

#### `Action`

``` purescript
data Action
  = Init 
  | Sorting 
  | UploadFile 
  | MountDB 
  | CreateNotebook 
  | CreateFolder 
```

Output messages

#### `State`

``` purescript
type State = { sort :: Sort }
```

sort direction in list (used in chevron direction right now only)

#### `view`

``` purescript
view :: Receiver Action _ -> State -> Eff _ VTree
```


#### `foldState`

``` purescript
foldState :: Action -> State -> Eff _ State
```



## Module View.User


This module will be removed, used only for layout

#### `Action`

``` purescript
data Action
  = Init 
```


#### `State`

``` purescript
newtype State
  = State String
```


#### `view`

``` purescript
view :: Receiver Action _ -> State -> Eff _ VTree
```


#### `foldState`

``` purescript
foldState :: Action -> State -> Eff _ State
```



## Module VirtualDOM.Events


Shortcuts for VirtualDOM events
They are not well typed, so it's possible
to put in "onclick" everything
it works when we put  Eff _ Unit there or
Callback defined here

#### `Callback`

``` purescript
data Callback :: *
```


#### `Handler`

``` purescript
type Handler e = Node -> Event -> Eff e Unit
```

Will be changed in favor of HTMLElement, I suppose

#### `mkCallback`

``` purescript
mkCallback :: forall e. Handler e -> Callback
```


#### `returnFalse`

``` purescript
returnFalse :: Callback
```

emptyHandler

#### `HookFn`

``` purescript
type HookFn = Fn2 Node String (Eff (dom :: DOM) Unit)
```

#### `Listener`

``` purescript
type Listener = { callback :: Callback, event :: String }
```


#### `listener`

``` purescript
listener :: forall e. String -> (Event -> Eff e Unit) -> Listener
```


#### `mkCb`

``` purescript
mkCb :: forall e. (Event -> Eff e Unit) -> Callback
```


#### `listen`

``` purescript
listen :: Listener -> HookFn
```


#### `ignore`

``` purescript
ignore :: Listener -> HookFn
```


#### `emptyHook`

``` purescript
emptyHook :: VHook
```

#### `composeHooks`

``` purescript
composeHooks :: VHook -> VHook -> VHook
```



## Module Control.Reactive.Event


Custom event module. Probably would be moved to ```simple-dom```
or other package

#### `Event`

``` purescript
data Event :: *
```


#### `target`

``` purescript
target :: forall e. Event -> Eff e HTMLElement
```


#### `detail`

``` purescript
detail :: forall e a. Event -> Eff e a
```


#### `raiseEventImpl`

``` purescript
raiseEventImpl :: forall e a. Fn3 String HTMLElement a (Eff (dom :: DOM | e) HTMLElement)
```


#### `raiseEvent`

``` purescript
raiseEvent :: forall e a. String -> HTMLElement -> a -> Eff (dom :: DOM | e) HTMLElement
```



## Module Control.Reactive.File


Uploading file module probably will be moved out to separate project

#### `FileReader`

``` purescript
data FileReader :: *
```

prependFileUploader "li" {} [vtext "foo"] = 
 vnode "li" {} [input {type: "file"}]

#### `File`

``` purescript
data File :: *
```


#### `FileList`

``` purescript
data FileList :: *
```


#### `fileListToArray`

``` purescript
fileListToArray :: FileList -> [File]
```


#### `name`

``` purescript
name :: forall e. File -> Eff e String
```


#### `file2blob`

``` purescript
file2blob :: File -> Blob
```


#### `newFormData`

``` purescript
newFormData :: forall e. Eff e FormData
```


#### `append2FormData`

``` purescript
append2FormData :: forall e a. String -> a -> FormData -> Eff e FormData
```


#### `newReader`

``` purescript
newReader :: forall e. Eff e FileReader
```


#### `readAsBinaryString`

``` purescript
readAsBinaryString :: forall e. File -> FileReader -> Eff e Unit
```


#### `str2blob`

``` purescript
str2blob :: forall e. String -> Blob
```


#### `onload`

``` purescript
onload :: forall e e'. (Event -> Eff e Unit) -> FileReader -> Eff e' FileReader
```


#### `result`

``` purescript
result :: forall e. FileReader -> Eff e (Maybe String)
```


#### `files`

``` purescript
files :: forall e. HTMLElement -> Eff e FileList
```


#### `uploader`

``` purescript
uploader :: forall p a. String -> { click :: VHook | p } -> [VTree] -> VTree
```




