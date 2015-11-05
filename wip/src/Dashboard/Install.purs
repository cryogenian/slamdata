module Dashboard.Install where

import Prelude

import Control.Monad.Free (Free())
import Dashboard.Common (Slam())
import Dashboard.Dialog as Dialog
import Dashboard.Navbar as Navbar
import Dashboard.Notebook as Notebook
import Dashboard.Query
import Dashboard.State
import Data.Either (Either())
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)
import Halogen
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))


type ChildState =
  Either Notebook.StateP
  (Either Navbar.StateP
   Dialog.StateP)

type ChildQuery =
  Coproduct Notebook.QueryP
  (Coproduct Navbar.QueryP
   Dialog.QueryP)

data NotebookSlot = NotebookSlot
derive instance genericNotebookSlot :: Generic NotebookSlot
instance eqNoteboolSlot :: Eq NotebookSlot where eq = gEq
instance ordNotebookSlot :: Ord NotebookSlot where compare = gCompare

data NavbarSlot = NavbarSlot
derive instance genericNavbarSlot :: Generic NavbarSlot
instance eqNavbarSlot :: Eq NavbarSlot where eq = gEq
instance ordNavbarSlot :: Ord NavbarSlot where compare = gCompare

data DialogSlot = DialogSlot
derive instance genericDialogSlot :: Generic DialogSlot
instance eqDialogSlot :: Eq DialogSlot where eq = gEq
instance ordDialogSlot :: Ord DialogSlot where compare = gCompare

type ChildSlot =
  Either NotebookSlot
  (Either NavbarSlot
   DialogSlot)

cpNotebook :: ChildPath
              Notebook.StateP ChildState
              Notebook.QueryP ChildQuery
              NotebookSlot ChildSlot
cpNotebook = cpL

cpNavbar :: ChildPath
            Navbar.StateP ChildState
            Navbar.QueryP ChildQuery
            NavbarSlot ChildSlot
cpNavbar = cpR :> cpL

cpDialog :: ChildPath
            Dialog.StateP ChildState
            Dialog.QueryP ChildQuery
            DialogSlot ChildSlot
cpDialog = cpR :> cpR


type StateP = InstalledState State ChildState Query ChildQuery Slam ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)
type Algebra =
  Free (HalogenF State Query
        (QueryF State ChildState Query ChildQuery Slam ChildSlot))
