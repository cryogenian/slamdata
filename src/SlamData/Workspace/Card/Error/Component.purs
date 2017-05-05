{-
Copyright 2017 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.Workspace.Card.Error.Component
  ( errorCardComponent
  , module SlamData.Workspace.Card.Error.Component.Query
  , module SlamData.Workspace.Card.Error.Component.State
  ) where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Path.Pathy as Path
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Quasar.Advanced.QuasarAF as QA
import SlamData.GlobalError as GE
import SlamData.Monad (Slam)
import SlamData.Render.Icon as I
import SlamData.Wiring as Wiring
import SlamData.Workspace.AccessType (AccessType(..))
import SlamData.Workspace.Card.CardType (CardType(..), AceMode(..), cardName)
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Error (CardError(..), cardToGlobalError)
import SlamData.Workspace.Card.Error.Component.Query (Query(..))
import SlamData.Workspace.Card.Error.Component.State (State, initialState)
import Utils (prettyJson)

type DSL = H.ComponentDSL State Query Void Slam
type HTML = H.ComponentHTML Query

errorCardComponent ∷ H.Component HH.HTML Query CardError Void Slam
errorCardComponent =
  H.lifecycleComponent
    { initialState: initialState
    , render
    , eval
    -- The input of this card is never modified after it is created, we only
    -- use the input for the initial state.
    , receiver: const Nothing
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }

render ∷ State → HTML
render st =
  HH.div
    [ HP.class_ (H.ClassName "sd-error-container") ]
    [ prettyPrintCardError st st.error ]

errorTitle ∷ Array HTML → HTML
errorTitle title =
  HH.h1_
    [ I.warningSm
    , HH.span_ title
    ]

eval ∷ Query ~> DSL
eval = case _ of
  Init next → do
    { accessType } ← Wiring.expose
    H.modify (_ { accessType = accessType })
    pure next
  ToggleExpanded b next → do
    H.modify (_ { expanded = b })
    pure next

printQErrorWithDetails ∷ QA.QError → HTML
printQErrorWithDetails = case _ of
  err → HH.text (QA.printQError err)

printQErrorDetails ∷ QA.QError → HTML
printQErrorDetails = case _ of
  QA.ErrorMessage { raw } → printQErrorRaw raw
  err → HH.text (QA.printQError err)

printQErrorRaw ∷ J.JObject → HTML
printQErrorRaw raw = HH.pre_ [ HH.text (prettyJson (J.fromObject raw)) ]

prettyPrintCardError ∷ State → CardError → HTML
prettyPrintCardError state ce = case cardToGlobalError ce of
  Just ge →
    HH.div_
      [ errorTitle [ HH.text (GE.print ge) ] ]
  Nothing → case ce of
    QuasarError qError →
      HH.div_
        [ errorTitle [ HH.text "An error occurred." ]
        , printQErrorWithDetails qError
        ]
    StringlyTypedError err →
      HH.div_
        [ errorTitle [ HH.text err ] ]
    CacheCardError cce → cacheErrorMessage state cce
    QueryCardError qce → queryErrorMessage state qce
    MarkdownCardError mde → markdownErrorMessage state mde
    DownloadOptionsCardError dloe → downloadOptionsErrorMessage state dloe
    OpenCardError oce → openErrorMessage state oce
    TableCardError tce → tableErrorMessage state tce
    FormInputStaticCardError fise → formInputStaticErrorMessage state fise
    FormInputLabeledCardError file → formInputLabeledErrorMessage state file

collapsible ∷ String → HTML → Boolean → HTML
collapsible title content expanded =
  HH.div
    [ HP.classes [ H.ClassName "sd-collapsible-error" ] ]
    $ join
      [ pure $
          HH.button
            [ HP.class_ (H.ClassName "sd-form-button")
            , HE.onClick $ HE.input_ (ToggleExpanded (not expanded))
            ]
            [ if expanded then I.chevronDownSm else I.chevronRightSm
            , HH.span_ [ HH.text title ]
            ]
      , guard expanded $>
          HH.div
            [ HP.class_ (H.ClassName "sd-collapsible-error-content") ]
            [ content ]
      ]

queryErrorMessage ∷ State → CE.QueryError → HTML
queryErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      HH.div_
        [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName (Ace SQLMode) <> " card, please notify the author of this workspace." ]
        , collapsible "Error details" (renderDetails err) expanded
        ]
  where
  renderDetails = case _ of
    CE.QueryCompileError qErr →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "An error occurred during query compilation." ]
          , renderMore qErr
          ]
    CE.QueryRetrieveResultError qErr →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "An error occurred when retrieving the query result." ]
          , renderMore qErr
          ]
  renderMore = case _ of
    QA.ErrorMessage {title, message, raw} →
      pure $ HH.div_
        $ join
          [ foldMap (\t -> pure $ HH.p_ [ HH.text t ]) title
          , pure $ HH.p_ [ HH.text message ]
          , guard (accessType == Editable) $> collapsible "Quasar error details" (printQErrorRaw raw) expanded
          ]
    err' →
      guard (accessType == Editable) $> collapsible "Quasar error details" (printQErrorDetails err') expanded

cacheErrorMessage ∷ State → CE.CacheError → HTML
cacheErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      HH.div_
        [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName Cache <> " card, please notify the author of this workspace." ]
        , collapsible "Error details" (renderDetails err) expanded
        ]
  where
  renderDetails = case _ of
    CE.CacheInvalidFilepath fp →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "There is a problem in the configuration of the " <> cardName Cache <> " card." ]
          , pure $ HH.p_
              [ HH.text "The provided path "
              , HH.code_ [ HH.text fp ]
              , HH.text " is not a valid location to store the cache result."
              ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card to fix this error." ]
          ]
    CE.CacheQuasarError qe →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "Caching the result of a query failed." ]
          , pure $ HH.p_ [ HH.text "The Quasar analytics engine returned an error while verifying the cache result." ]
          , guard (accessType == Editable) $> collapsible "Quasar error details" (printQErrorDetails qe) expanded
          ]
    CE.CacheErrorSavingFile fp →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "Caching the result of a query failed." ]
          , pure $ HH.p_
              [ HH.text "The file "
              , HH.code_ [ HH.text (Path.printPath fp) ]
              , HH.text " could not be written to."
              ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and provide an alternative path to fix this error." ]
          ]
    CE.CacheResourceNotModified →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "Caching the result of a query failed." ]
          , pure $ HH.p_ [ HH.text "Caching can only be applied to queries that perform at least one transformation on an existing data set." ]
          -- TODO: only show this solution when there are no following cards?
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and delete it to fix this error." ]
          ]

markdownErrorMessage ∷ State → CE.MarkdownError → HTML
markdownErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      HH.div_
        [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName (Ace MarkdownMode) <> " card, please notify the author of this workspace." ]
        , collapsible "Error details" (renderDetails err) expanded
        ]
  where
  -- TODO: explain which field for all these cases? -gb
  renderDetails = case _ of
    CE.MarkdownParseError {markdown, error} →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "Parsing the provided Markdown failed." ]
          , pure $ HH.p_
              [ HH.text "We encountered the following parse error: "
              , HH.code_ [ HH.text error ]
              ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card to fix this error." ]
          ]
    CE.MarkdownSqlParseError {sql, error} →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "Parsing SQL embedded inside Markdown failed." ]
          , pure $ renderParseError error sql
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card to fix this error." ]
          ]
    CE.MarkdownNoTextBoxResults →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "A text box field did not return any results." ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card to fix this error." ]
          ]
    CE.MarkdownInvalidTimeValue { time, error } →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "An invalid time value was provided." ]
          , pure $ renderParseError error time
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card to fix this error." ]
          ]
    CE.MarkdownInvalidDateValue { date, error } →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "An invalid date value was provided." ]
          , pure $ renderParseError error date
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card to fix this error." ]
          ]
    CE.MarkdownInvalidDateTimeValue { datetime, error } →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "An invalid timestamp value was provided." ]
          , pure $ renderParseError error datetime
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card to fix this error." ]
          ]
    CE.MarkdownTypeError t1 t2 →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "A type mismatch occurred when populating a field in the " <> cardName (Ace MarkdownMode) <> " card." ]
          , pure $ HH.p_
              [ HH.text "We encountered the following type:"
              , HH.br_
              , HH.code_ [ HH.text t1 ]
              , HH.text " where we expected:"
              , HH.br_
              , HH.code_ [ HH.text t2 ]
              ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card to fix this error." ]
          ]
    where
      renderParseError error value =
        HH.p_
          [ HH.text "We encountered the following error:"
          , HH.br_
          , HH.code_ [ HH.text error ]
          , HH.text " when trying to parse:"
          , HH.br_
          , HH.code_ [ HH.text value ]
          ]

downloadOptionsErrorMessage ∷ State → CE.DownloadOptionsError → HTML
downloadOptionsErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      HH.div_
        [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName DownloadOptions <> " card, please notify the author of this workspace." ]
        , collapsible "Error details" (renderDetails err) expanded
        ]
  where
  renderDetails = case _ of
    CE.DownloadOptionsFilenameRequired →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "No filename was provided in the " <> cardName DownloadOptions <> " card." ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card to fix this error." ]
          ]
    CE.DownloadOptionsFilenameInvalid fn →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "The filename provided in the " <> cardName DownloadOptions <> " card is invalid." ]
          , pure $ HH.p_
              [ HH.code_ [ HH.text fn ], HH.text " is not a valid filepath." ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card to fix this error." ]
          ]

openErrorMessage ∷ State → CE.OpenError → HTML
openErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      HH.div_
        [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName Open <> " card, please notify the author of this workspace." ]
        , collapsible "Error details" (renderDetails err) expanded
        ]
  where
  renderDetails = case _ of
    CE.OpenFileNotFound fp →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "A file that was selected in the " <> cardName Open <> " card could not be found." ]
          , pure $ HH.p_
              [ HH.code_ [ HH.text fp ], HH.text " does not exist." ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and make a new selection to fix this error." ]
          ]
    CE.OpenNoResourceSelected →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "No resource was selected in the " <> cardName Open <> " card." ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and select either a file or a variable to fix this error." ]
          ]
    CE.OpenNoFileSelected →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "The resource selected in the " <> cardName Open <> " card is of an invalid type" ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and select a file or variable to fix this error." ]
          ]

tableErrorMessage ∷ State → CE.TableError → HTML
tableErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      HH.div_
        [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName Table <> " card, please notify the author of this workspace." ]
        , collapsible "Error details" (renderDetails err) expanded
        ]
  where
  renderDetails = case _ of
    CE.TableMissingResourceInputError →
      HH.div_
        [ errorTitle [ HH.text $ "The " <> cardName Table <> " card requires data as an input." ] ]
    CE.TableCountQuasarError qErr →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "A error occured when counting the preview data." ]
          , pure $ printQErrorWithDetails qErr
          ]
    CE.TableSampleQuasarError qErr →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "A error occured during sampling of the preview data." ]
          , pure $ printQErrorWithDetails qErr
          ]

formInputStaticErrorMessage ∷ State → CE.FormInputStaticError → HTML
formInputStaticErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      HH.div_
        [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName (SetupFormInput FIT.Static) <> " card, please notify the author of this workspace." ]
        , collapsible "Error details" (renderDetails err) expanded
        ]
  where
  renderDetails = case _ of
    CE.FIStaticNoAxis →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "An error occured when setting up the " <> cardName (SetupFormInput FIT.Static) <> " card." ]
          , pure $ HH.p_
              [ HH.text "No axis was selected" ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and select an axis to fix this error." ]
          ]
    CE.FIStaticMissingAxis axis →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "An error occured when setting up the " <> cardName (SetupFormInput FIT.Static) <> " card." ]
          , pure $ HH.p_
              [ HH.text "The selected axis was not present in the data." ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card to fix this error." ]
          ]

formInputLabeledErrorMessage ∷ State → CE.FormInputLabeledError → HTML
formInputLabeledErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      let
        fit = extractType err
      in
        HH.div_
          [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName (SetupFormInput fit) <> " card, please notify the author of this workspace." ]
          , collapsible "Error details" (renderDetails err) expanded
          ]
  where
  extractType = case _ of
    CE.FILabeledNoAxisError fit → fit
    CE.FILabeledEmptyResourceError fit → fit
    CE.FILabeledTooManyEntries { formInputType } → formInputType
    CE.FILabeledTooManySelected { formInputType } → formInputType
    CE.FILabeledNonUniqueLabelError fit _ → fit
  renderDetails = case _ of
    CE.FILabeledNoAxisError fit →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "An error occured when setting up the " <> cardName (SetupFormInput fit) <> " card." ]
          , pure $ HH.p_
              [ HH.text "No axis was selected" ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and select an axis to fix this error." ]
          ]
    CE.FILabeledEmptyResourceError fit →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "An error occured when setting up the " <> cardName (SetupFormInput fit) <> " card." ]
          , pure $ HH.p_
              [ HH.text "The selected resource was empty." ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and select an axis to fix this error." ]
          ]
    CE.FILabeledTooManyEntries { formInputType, maximum, entryCount } →
      let
        errorText =
          "The " <> FIT.printFormInputType formInputType
          <> " form input can't take more than "
          <> show (FIT.maximumCountOfEntries formInputType)
          <> "entries, but there were: "
          <> show entryCount
          <> ". Please use 'limit' or 'group by'"
      in
        HH.div_
          $ join
            [ pure $ errorTitle [ HH.text $ "An error occured when setting up the " <> cardName (SetupFormInput formInputType) <> " card." ]
            , pure $ HH.p_
                [ HH.text errorText ]
            , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and select an axis to fix this error." ]
            ]
    CE.FILabeledTooManySelected { formInputType, maximum, selectedCount } →
      let
        errorText =
          "The " <> FIT.printFormInputType formInputType
          <> " form input can't have more than "
          <> show (FIT.maximumCountOfSelectedValues formInputType)
          <> " selected values, but there were: "
          <> show selectedCount
          <> ". Please, use another axis"
      in
        HH.div_
          $ join
            [ pure $ errorTitle [ HH.text $ "An error occured when setting up the " <> cardName (SetupFormInput formInputType) <> " card." ]
            , pure $ HH.p_
                [ HH.text errorText ]
            , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and select an axis to fix this error." ]
            ]
    -- TODO: Use the label argument for a better error message
    CE.FILabeledNonUniqueLabelError fit label →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "An error occured when setting up the " <> cardName (SetupFormInput fit) <> " card." ]
          , pure $ HH.p_
              [ HH.text "Labels must be unique. Please, use other axis." ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and select an axis to fix this error." ]
          ]
