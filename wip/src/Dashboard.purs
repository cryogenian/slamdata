module Dashboard
       ( comp
       , module Dashboard.State
       , module Dashboard.Query
       ) where

import Prelude

import Control.Alt ((<|>))
import Dashboard.Common (Slam())
import Dashboard.Notebook as Notebook
import Dashboard.Dialog as Dialog
import Dashboard.Navbar as Navbar
import Dashboard.Install
import Dashboard.State (State(..), _editable, initialState)
import Dashboard.Query
import Data.Lens ((^.))
import Data.Maybe (fromMaybe)
import Halogen
import Halogen.Component.Utils (applyCF, forceRerender, forceRerender')
import Halogen.Component.ChildPath (prjSlot, prjQuery)
import Halogen.CustomProps as Cp
import Halogen.HTML as H
import Halogen.HTML.Elements as H
import Halogen.HTML.Events as E
import Halogen.HTML.Events.Forms as E
import Halogen.HTML.Events.Handler as E
import Halogen.HTML.Properties as P
import Render.CssClasses as Rc

comp :: Component StateP QueryP Slam
comp = parentComponent' render eval peek

render :: RenderParent State ChildState Query ChildQuery Slam ChildSlot
render state@(State r) =
  H.div [ P.classes classes ]
  [ H.slot' cpNavbar NavbarSlot
    \_ -> { component: Navbar.comp
          , initialState: Navbar.initialState
          }
  , H.slot' cpNotebook NotebookSlot
    \_ -> { component: Notebook.comp
          , initialState: Notebook.initialState
          }
  , H.slot' cpDialog DialogSlot
    \_ -> { component: Dialog.comp
          , initialState: Dialog.initialState
          }
  ]
  where
  classes = if not (state ^. _editable)
            then [ Rc.dashboardViewHack ]
            else [ ]

eval :: EvalParent Query State ChildState Query ChildQuery Slam ChildSlot
eval (Query next) = pure next


peek :: forall a. ChildF ChildSlot ChildQuery a -> Algebra Unit
peek (ChildF p q) = do
  fromMaybe (pure unit)
  $   (navbarPeek <$> prjSlot cpNavbar p <*> prjQuery cpNavbar q)
  <|> (notebookPeek <$> prjSlot cpNotebook p <*> prjQuery cpNotebook q)
  <|> (dialogPeek <$> prjSlot cpDialog p <*> prjQuery cpDialog q)

navbarPeek :: forall a. NavbarSlot -> Navbar.QueryP a -> Algebra Unit
navbarPeek p q = pure unit

notebookPeek :: forall a. NotebookSlot -> Notebook.QueryP a -> Algebra Unit
notebookPeek p q = pure unit

dialogPeek :: forall a. DialogSlot -> Dialog.QueryP a -> Algebra Unit
dialogPeek p q = pure unit
