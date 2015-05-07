module Driver.Notebook.Search where

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Events.Monad as E

sqlFromSearch :: forall e i. String -> [ H.HTML (E.Event e i) ]
sqlFromSearch input =
  [ H.p_ [ H.text $ "select * from path where interpreted" <> input  ] ]
