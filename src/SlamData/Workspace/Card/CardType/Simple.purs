module SlamData.Workspace.Card.CardType.Simple where

import SlamData.Prelude

import Data.Variant (inj, on)

import Halogen.HTML as H

import Unsafe.Coerce (unsafeCoerce)

_search = SProxy ∷ SProxy "search"
_chart = SProxy ∷ SProxy "chart"
_form = SProxy ∷ SProxy "form"
_markdown = SProxy ∷ SProxy "markdown"
_table = SProxy ∷ SProxy "table"
_download = SProxy ∷ SProxy "download"
_variables = SProxy ∷ SProxy "variables"
_troubleshoot = SProxy ∷ SProxy "troubleshoot"
_cache = SProxy ∷ SProxy "cache"
_open = SProxy ∷ SProxy "open"
_downloadOptions = SProxy ∷ SProxy "downloadOptions"
_draftboard = SProxy ∷ SProxy "draftboard"
_tabs = SProxy ∷ SProxy "tabs"
_structureEditor = SProxy ∷ SProxy "structureEditor"
_geo = SProxy ∷ SProxy "geo"

search ∷ ∀ r. Variant (search ∷ Unit|r)
search = inj _search unit

chart ∷ ∀ r. Variant (chart ∷ Unit|r)
chart = inj _chart unit

form ∷ ∀ r. Variant (form ∷ Unit|r)
form = inj _form unit

markdown ∷ ∀ r. Variant (markdown ∷ Unit|r)
markdown = inj _markdown unit

table ∷ ∀ r. Variant (table ∷ Unit|r)
table = inj _table unit

download ∷ ∀ r. Variant (download ∷ Unit|r)
download = inj _download unit

variables ∷ ∀ r. Variant (variables ∷ Unit|r)
variables = inj _variables unit

troubleshoot ∷ ∀ r. Variant (troubleshoot ∷ Unit|r)
troubleshoot = inj _troubleshoot unit

cache ∷ ∀ r. Variant (cache ∷ Unit|r)
cache = inj _cache unit

open ∷ ∀ r. Variant (open ∷ Unit|r)
open = inj _open unit

downloadOptions ∷ ∀ r. Variant (downloadOptions ∷ Unit|r)
downloadOptions = inj _downloadOptions unit

draftboard ∷ ∀ r. Variant (draftboard ∷ Unit|r)
draftboard = inj _draftboard unit

tabs ∷ ∀ r. Variant (tabs ∷ Unit|r)
tabs = inj _tabs unit

structureEditor ∷ ∀ r. Variant (structureEditor ∷ Unit|r)
structureEditor = inj _structureEditor unit

geo ∷ ∀ r. Variant (geo ∷ Unit|r)
geo = inj _geo unit


type SimpleR r =
  ( search ∷ Unit
  , chart ∷ Unit
  , form ∷ Unit
  , markdown ∷ Unit
  , table ∷ Unit
  , download ∷ Unit
  , variables ∷ Unit
  , troubleshoot ∷ Unit
  , cache ∷ Unit
  , open ∷ Unit
  , downloadOptions ∷ Unit
  , draftboard ∷ Unit
  , tabs ∷ Unit
  , structureEditor ∷ Unit
  , geo ∷ Unit
  | r)

type Simple r = Variant (SimpleR r)

eq_ ∷ ∀ r rr. (Variant r → Variant rr → Boolean) → Simple r → Simple rr → Boolean
eq_ cb r = cb (unsafeCoerce r)
  # on _search (on _search tt ff r)
  # on _chart (on _chart tt ff r)
  # on _form (on _form tt ff r)
  # on _markdown (on _markdown tt ff r)
  # on _table (on _table tt ff r)
  # on _download (on _download tt ff r)
  # on _variables (on _variables tt ff r)
  # on _troubleshoot (on _troubleshoot tt ff r)
  # on _cache (on _cache tt ff r)
  # on _open (on _open tt ff r)
  # on _downloadOptions (on _downloadOptions tt ff r)
  # on _draftboard (on _draftboard tt ff r)
  # on _tabs (on _tabs tt ff r)
  # on _structureEditor (on _structureEditor tt ff r)
  # on _geo (on _geo tt ff r)


print ∷ ∀ r. (Variant r → String) → Simple r → String
print cb = cb
  # on _search (const "search")
  # on _chart (const "chart")
  # on _form (const "form-input")
  # on _markdown (const "markdown")
  # on _table (const "table")
  # on _structureEditor (const "structure-editor")
  # on _download (const "download")
  # on _downloadOptions (const "download-options")
  # on _draftboard (const "draftboard")
  # on _variables (const "variables")
  # on _troubleshoot (const "troubleshoot")
  # on _cache (const "cache")
  # on _open (const "open")
  # on _tabs (const "tabs")
  # on _geo (const "geo-chart")


parse ∷ ∀ r. String → String ⊹ Simple r
parse = case _ of
  "search" → pure search
  "chart" → pure chart
  "form-input" → pure form
  "markdown" → pure markdown
  "table" → pure table
  "download" → pure download
  "variables" → pure variables
  "troubleshoot" → pure troubleshoot
  "cache" → pure cache
  "open" → pure open
  "download-options" → pure downloadOptions
  "draftboard" → pure draftboard
  "tabs" → pure tabs
  "structure-editor" → pure structureEditor
  "geo-chart" → pure geo
  _ → Left "This is not basic card type"

name ∷ ∀ r. (Variant r → String) → Simple r → String
name cb = cb
  # on _search (const "Search")
  # on _chart (const "Show Chart")
  # on _geo (const "Show Geo Chart")
  # on _form (const "Show Form")
  # on _markdown (const "Show Markdown")
  # on _table (const "Preview Table")
  # on _download (const "Show Download")
  # on _variables (const "Setup Variables")
  # on _troubleshoot (const "Troubleshoot")
  # on _cache (const "Cache")
  # on _open (const "Open")
  # on _downloadOptions (const "Setup Downlaod")
  # on _draftboard (const "Setup Dashboard")
  # on _tabs (const "Setup Tabs")
  # on _structureEditor (const "Structure Editor")

icon ∷ ∀ r. (Variant r → String) → Simple r → String
icon cb = cb
  # on _search (const "search")
  # on _download (const "showDownload")
  # on _variables (const "setupVariables")
  # on _troubleshoot (const "troubleshoot")
  # on _chart (const "showChart")
  # on _form (const "showFormInput")
  # on _markdown (const "showMarkdown")
  # on _table (const "table")
  # on _cache (const "cache")
  # on _open (const "open")
  # on _downloadOptions (const "downloadOptions")
  # on _draftboard (const "dashboard")
  # on _tabs (const "tabs")
  # on _structureEditor (const "structureEditor")
  # on _geo (const "geoChart")

cardClasses ∷ ∀ r. (Variant r → Array H.ClassName) → Simple r → Array H.ClassName
cardClasses cb = cb
  # on _search (\_ → [ H.ClassName "sd-card-search" ])
  # on _chart (\_ → [ H.ClassName "sd-card-chart" ] )
  # on _geo (\_ → [ H.ClassName "sd-card-geo-chart" ])
  # on _form (\_ → [ H.ClassName "sd-card-form-input" ])
  # on _markdown (\_ → [ H.ClassName "sd-card-markdown" ])
  # on _table (\_ → [ H.ClassName "sd-card-table" ])
  # on _download (\_ → [ H.ClassName "sd-card-download" ])
  # on _downloadOptions (\_ → [ H.ClassName "sd-card-download-options" ])
  # on _variables (\_ → [ H.ClassName "sd-card-variables" ])
  # on _troubleshoot (\_ → [ H.ClassName "sd-card-troubleshoot" ])
  # on _cache (\_ → [ H.ClassName "sd-card-cache" ] )
  # on _open (\_ → [ H.ClassName "sd-card-open" ])
  # on _draftboard (\_ → [ H.ClassName "sd-card-draftboard" ])
  # on _structureEditor (\_ → [ H.ClassName "sd-structure-editor" ])
  # on _tabs (\_ → [ H.ClassName "sd-card-tabs" ])

consumerInteractable ∷ ∀ r. (Variant r → Boolean) → Simple r → Boolean
consumerInteractable cb = cb
  # on _search tt
  # on _geo tt
  # on _chart tt
  # on _form tt
  # on _markdown tt
  # on _table tt
  # on _download tt
  # on _downloadOptions ff
  # on _variables ff
  # on _troubleshoot ff
  # on _cache ff
  # on _open ff
  # on _draftboard tt
  # on _tabs tt
  # on _structureEditor ff
