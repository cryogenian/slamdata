module View.File.Modal.Common where

import Control.Functor (($>))
import Control.Plus (empty)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.HTML.Events as E 

h4 :: forall p i. String -> [H.HTML p i]
h4 str = [ H.h4_ [H.text str] ]

section :: forall p i. [A.ClassName] -> [H.HTML p i] -> H.HTML p i
section clss = H.div [A.classes clss]

header :: forall p i. [H.HTML p i] -> H.HTML p i
header = section [B.modalHeader]

body :: forall p i. [H.HTML p i] -> H.HTML p i
body = section [B.modalBody]

footer :: forall p i. [H.HTML p i] -> H.HTML p i
footer = section [B.modalFooter]

nonSubmit :: forall i e. A.Attr ((E.Event e) i)
nonSubmit = E.onSubmit (\_ -> E.preventDefault $> empty)

