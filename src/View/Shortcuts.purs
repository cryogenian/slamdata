module View.Shortcuts where

import VirtualDOM
import VirtualDOM.VTree

div = vnode "div"
nav = vnode "nav"
a = vnode "a"
form = vnode "form"
input = vnode "input"
i = vnode "i"
span = vnode "span"
button = vnode "button"
ul = vnode "ul"
ol = vnode "ol"
li = vnode "li"
emptyVTree = vnode "" {} []
jsVoid = "javascript:void(0);"
