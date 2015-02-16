module View.Shortcuts where

import VirtualDOM
import VirtualDOM.VTree


div :: forall props. {  | props } -> [VTree] -> VTree
div = vnode "div"

nav :: forall props. {  | props } -> [VTree] -> VTree
nav = vnode "nav"

a :: forall props. {  | props } -> [VTree] -> VTree
a = vnode "a"

form :: forall props. {  | props } -> [VTree] -> VTree
form = vnode "form"

input :: forall props. {  | props } -> [VTree] -> VTree
input = vnode "input"

i :: forall props. {  | props } -> [VTree] -> VTree
i = vnode "i"

span :: forall props. {  | props } -> [VTree] -> VTree
span = vnode "span"

button :: forall props. {  | props } -> [VTree] -> VTree
button = vnode "button"

ul :: forall props. {  | props } -> [VTree] -> VTree
ul = vnode "ul"

ol :: forall props. {  | props } -> [VTree] -> VTree
ol = vnode "ol"

li :: forall props. {  | props } -> [VTree] -> VTree
li = vnode "li"

emptyVTree :: VTree
emptyVTree = vnode "" {} []

jsVoid :: String
jsVoid = "javascript:void(0);"
