module Model.File.Dialog.Rename where

import Data.Maybe (Maybe(..))
import Model.Resource
import Optic.Core (lens, LensP())

newtype RenameDialogRec = RenameDialogRec
  { showList :: Boolean
  , initial :: Resource
  , resource :: Resource
  , dirs :: [Resource]
  , dir :: Resource
  , siblings :: [Resource]
  , error :: Maybe String
  }

initialRenameDialog :: Resource -> RenameDialogRec
initialRenameDialog resource = RenameDialogRec
  { showList: false
  , initial: resource
  , resource: resource
  , dir: parent resource
  , siblings: []
  , dirs: []
  , error: Nothing
  }

_renameDialogRec :: LensP RenameDialogRec _
_renameDialogRec = lens (\(RenameDialogRec obj) -> obj) (const RenameDialogRec)

_showList :: LensP RenameDialogRec Boolean
_showList = _renameDialogRec <<< lens _.showList (_ { showList = _ })

_initial :: LensP RenameDialogRec Resource
_initial = _renameDialogRec <<< lens _.initial (_ { initial = _ })

_resource :: LensP RenameDialogRec Resource
_resource = _renameDialogRec <<< lens _.resource (_ { resource = _ })

_dirs :: LensP RenameDialogRec [Resource]
_dirs = _renameDialogRec <<< lens _.dirs (_ { dirs = _ })

_dir :: LensP RenameDialogRec Resource
_dir = _renameDialogRec <<< lens _.dir (_ { dir = _ })

_siblings :: LensP RenameDialogRec [Resource]
_siblings = _renameDialogRec <<< lens _.siblings (_ { siblings = _ })

_error :: LensP RenameDialogRec (Maybe String)
_error = _renameDialogRec <<< lens _.error (_ { error = _ })
