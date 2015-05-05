module View.File.Modal.Common where

import Control.Functor (($>))
import Control.Plus (empty)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.HTML.Events as E 

h4 :: forall i. String -> [H.HTML i]
h4 str = [ H.h4_ [H.text str] ]

section :: forall i. [A.ClassName] -> [H.HTML i] -> H.HTML i
section clss = H.div [A.classes clss]

header :: forall i. [H.HTML i] -> H.HTML i
header = section [B.modalHeader]

body :: forall i. [H.HTML i] -> H.HTML i
body = section [B.modalBody]

footer :: forall i. [H.HTML i] -> H.HTML i
footer = section [B.modalFooter]

nonSubmit :: forall i e. A.Attr ((E.Event e) i)
nonSubmit = E.onSubmit (\_ -> E.preventDefault $> empty)

