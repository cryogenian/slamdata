module SlamData.Render.Icon
  ( eyeHidden
  , eyeVisible
  , chevronDownSm
  , chevronLeftSm
  , chevronRightSm
  , chevronUpSm
  , copySm
  , documentSm
  , file
  , searchSm
  , shareSm
  , trashCanSm
  , zoomInSm
  , zoomOutSm
  , cog
  , workspaceSm
  , workspace
  , exchangeSm
  , playerFastForward
  , playerNext
  , playerPlay
  , playerPrevious
  , playerRewind
  , tagSm
  , tagsSm
  , wrenchesCrossed
  , databaseCreate
  , database
  , folderCreate
  , folderSm
  , folder
  , cloudDownload
  , cloudUpload
  , cloudDownloadSm
  ) where

import SlamData.Prelude

import Halogen.HTML as H
import Halogen.HTML.Properties as P

iconHelper ∷ ∀ p i. String → H.HTML p i
iconHelper s =
  let
    svgElem = H.elementNS $ H.Namespace "http://www.w3.org/2000/svg"
    xlinkAttr = H.attrNS $ H.Namespace "http://www.w3.org/1999/xlink"
  in
    -- oddly, I suppose do to namespacing, the CSS class on <svg> not
    -- picked up wrapping it seems to work though
    H.i
      [ P.class_ $ H.ClassName $ "sd-icon sd-icon--" <> s ]
      [ svgElem (H.ElemName "svg")
        []
        [ svgElem (H.ElemName "use")
          [ xlinkAttr (H.AttrName "xlink:href") $ "#sd-icon--" <> s ]
          []
        ]
      ]

eyeHidden ∷ ∀ p i. H.HTML p i
eyeHidden = iconHelper "eye-hidden"

eyeVisible ∷ ∀ p i. H.HTML p i
eyeVisible = iconHelper "eye-visible"

chevronDownSm ∷ ∀ p i. H.HTML p i
chevronDownSm = iconHelper "chevron-down-sm"

chevronLeftSm ∷ ∀ p i. H.HTML p i
chevronLeftSm = iconHelper "chevron-left-sm"

chevronRightSm ∷ ∀ p i. H.HTML p i
chevronRightSm = iconHelper "chevron-right-sm"

chevronUpSm ∷ ∀ p i. H.HTML p i
chevronUpSm = iconHelper "chevron-up-sm"

copySm ∷ ∀ p i. H.HTML p i
copySm = iconHelper "copy-sm"

documentSm ∷ ∀ p i. H.HTML p i
documentSm = iconHelper "document-sm"

file ∷ ∀ p i. H.HTML p i
file = iconHelper "file"

searchSm ∷ ∀ p i. H.HTML p i
searchSm = iconHelper "search-sm"

shareSm ∷ ∀ p i. H.HTML p i
shareSm = iconHelper "share-sm"

trashCanSm ∷ ∀ p i. H.HTML p i
trashCanSm = iconHelper "trash-can-sm"

zoomInSm ∷ ∀ p i. H.HTML p i
zoomInSm = iconHelper "zoom-in-sm"

zoomOutSm ∷ ∀ p i. H.HTML p i
zoomOutSm = iconHelper "zoom-out-sm"

cog ∷ ∀ p i. H.HTML p i
cog = iconHelper "cog"

workspaceSm ∷ ∀ p i. H.HTML p i
workspaceSm = iconHelper "workspace-sm"

workspace ∷ ∀ p i. H.HTML p i
workspace = iconHelper "workspace"

exchangeSm ∷ ∀ p i. H.HTML p i
exchangeSm = iconHelper "exchange-sm"

playerFastForward ∷ ∀ p i. H.HTML p i
playerFastForward = iconHelper "player-fast-forward"

playerNext ∷ ∀ p i. H.HTML p i
playerNext = iconHelper "player-next"

playerPlay ∷ ∀ p i. H.HTML p i
playerPlay = iconHelper "player-play"

playerPrevious ∷ ∀ p i. H.HTML p i
playerPrevious = iconHelper "player-previous"

playerRewind ∷ ∀ p i. H.HTML p i
playerRewind = iconHelper "player-rewind"

tagSm ∷ ∀ p i. H.HTML p i
tagSm = iconHelper "tag-sm"

tagsSm ∷ ∀ p i. H.HTML p i
tagsSm = iconHelper "tags-sm"

wrenchesCrossed ∷ ∀ p i. H.HTML p i
wrenchesCrossed = iconHelper "wrenches-crossed"

databaseCreate ∷ ∀ p i. H.HTML p i
databaseCreate = iconHelper "database-create"

database ∷ ∀ p i. H.HTML p i
database = iconHelper "database"

folderCreate ∷ ∀ p i. H.HTML p i
folderCreate = iconHelper "folder-create"

folderSm ∷ ∀ p i. H.HTML p i
folderSm = iconHelper "folder-sm"

folder ∷ ∀ p i. H.HTML p i
folder = iconHelper "folder"

cloudDownload ∷ ∀ p i. H.HTML p i
cloudDownload = iconHelper "cloud-download"

cloudUpload ∷ ∀ p i. H.HTML p i
cloudUpload = iconHelper "cloud-upload"

cloudDownloadSm ∷ ∀ p i. H.HTML p i
cloudDownloadSm = iconHelper "cloud-download-sm"
