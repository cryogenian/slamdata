module View.File.Modal.Common where

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.Themes.Bootstrap3 as B

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
