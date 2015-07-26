module View.Modal.Common where

import Prelude 
import Data.Functor (($>))
import Control.Plus (empty)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B

h4 :: forall i. String -> Array (H.HTML i)
h4 str = [ H.h4_ [H.text str] ]

section :: forall i. Array (A.ClassName) -> Array (H.HTML i) -> H.HTML i
section clss = H.div [A.classes clss]

header :: forall i. Array (H.HTML i) -> H.HTML i
header = section [B.modalHeader]

body :: forall i. Array (H.HTML i) -> H.HTML i
body = section [B.modalBody]

footer :: forall i. Array (H.HTML i) -> H.HTML i
footer = section [B.modalFooter]

nonSubmit :: forall i e. A.Attr ((E.Event e) i)
nonSubmit = E.onSubmit (\_ -> E.preventDefault $> empty)

