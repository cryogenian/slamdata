{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Workspace.Deck.Dialog.Share.Component where

import SlamData.Prelude

import Control.UI.Browser (select)

import Data.Array as Arr
import Data.String as Str
import Data.Path.Pathy (rootDir, (</>), file)
import Data.Path.Pathy as Pt

import DOM.HTML.Types (HTMLElement, htmlElementToElement)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.CustomProps as Cp
import Halogen.Themes.Bootstrap3 as B
import Halogen.Component.Utils (raise)

import Quasar.Advanced.Types as QT

import SlamData.Quasar.Security as Q
import SlamData.Monad (Slam)
import SlamData.Render.Common (glyph)
import SlamData.Workspace.Deck.Dialog.Share.Model (ShareResume(..), sharingActions, SharingInput)

import Utils.Path (rootFile, FilePath)

import ZClipboard as Z

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Slam

data SubjectType
  = User
  | Group
  | Token

derive instance subjectTypeEq ∷ Eq SubjectType

data ErrorType
  = Connection
  | Validation
  | GroupList

derive instance errorTypeEq ∷ Eq ErrorType

-- This is utility function that should be used only in
-- select/option stuff
readSubjectType ∷ String → SubjectType
readSubjectType "user" = User
readSubjectType "group" = Group
readSubjectType "token" = Token
readSubjectType _ = User

-- Same with `readSubjectType`
readShareResume ∷ String → ShareResume
readShareResume "edit" = Edit
readShareResume _ = View

printShareResume ∷ ShareResume → String
printShareResume Edit = "Edit"
printShareResume View = "View"

-- It would be better if all this Boolean stuff was replaced with
-- some kind of ADT abstracting automata.
type State =
  { subjectType ∷ SubjectType
  , error ∷ Maybe ErrorType
  , email ∷ String
    -- Actually should be Group from purescript-quasar
  , groups ∷ Array FilePath
  , groupSelected ∷ Maybe FilePath
  , tokenName ∷ String
  , loading ∷ Boolean
  , showError ∷ Boolean
  , submitting ∷ Boolean
  , shareResume ∷ ShareResume
  , tokenSecret ∷ Maybe String
  , sharingInput ∷ SharingInput
  }

initialState ∷ SharingInput → State
initialState sharingInput =
  { subjectType: User
  , error: Nothing
  , email: ""
  , groups: [ ]
  , groupSelected: Nothing
  , tokenName: ""
  , loading: true
  , showError: false
  , submitting: false
  , shareResume: View
  , tokenSecret: Nothing
  , sharingInput
  }

data Query a
  = Dismiss a
  | Share a
  | ChangeSubjectType SubjectType a
  | EmailChanged String a
  | DismissError a
  | Init a
  | GroupSelected String a
  | TokenNameChanged String a
  | ChangeShareResume ShareResume a
  | InitZClipboard String (Maybe HTMLElement) a
  | SelectElement HTMLElement a

comp ∷ H.Component State Query Slam
comp =
  H.lifecycleComponent
    { render
    , eval
    , finalizer: Nothing
    , initializer: Just (H.action Init)
    }

render ∷ State → HTML
render state =
  HH.div
    [ HP.classes [ HH.className "deck-dialog-share" ] ]
    $ (case state.tokenSecret of
          Nothing → [ HH.h4_ [ HH.text "Share deck" ] ]
          Just _ →
            [ HH.h4_ [ HH.text if state.tokenName ≡ ""
                               then "Untitled token"
                               else state.tokenName ⊕ " token"
                     ]
            , HH.h5_ [ HH.text $ printShareResume state.shareResume ]
            ])
  ⊕ [ HH.div
       [ HP.classes [ HH.className "deck-dialog-body" ]
       , HE.onClick ( HE.input_ DismissError )
       ]
       $ [ HH.div
             [ HP.classes
                 $ [ B.alert, B.alertInfo, HH.className "share-loading" ]
                 ⊕ if state.loading then [ ] else [ B.hidden ]
             ]
             [ HH.img [ HP.src "img/blue-spin.svg" ]
             , HH.text "Loading"
             ]
         ]
       ⊕ (flip foldMap state.tokenSecret \token →
           [ HH.form
               [ Cp.nonSubmit
               , HP.classes $ if state.loading then [ B.hidden ] else [ ]
               ]

               [ HH.div
                   [ HP.classes [ B.inputGroup ]
                   , HE.onClick $ HE.input (SelectElement ∘ _.target)
                   ]
                   [ HH.input
                       [ HP.classes [ B.formControl ]
                       , HP.value token
                       , HP.readonly true
                       , HP.title "Token secret"
                       ]
                   , HH.span
                       [ HP.classes [ B.inputGroupBtn ] ]
                       [ HH.button
                           [ HP.classes [ B.btn, B.btnDefault ]
                           , HP.ref (H.action ∘ InitZClipboard token)
                           , HP.id_ "copy-button"
                           ]
                           [ glyph B.glyphiconCopy ]
                       ]
                   ]
               ]
           ])
       ⊕ [ HH.form
             [ Cp.nonSubmit
             , HP.classes if isJust state.tokenSecret ∨ state.loading then [ B.hidden ] else [ ]
             ]
             [ HH.label
                 [ HP.for "subject-type" ]
                 [ HH.text "Subject type"
                 , HH.select
                     [ HP.classes [ B.formControl ]
                     , HP.id_ "subject-type"
                     , HE.onValueChange (HE.input (ChangeSubjectType ∘ readSubjectType))
                     , HP.disabled state.submitting
                     ]
                     $ [ HH.option
                          [ HP.value "user" ]
                          [ HH.text "User" ]
                       ]
                     ⊕ (guard (not $ Arr.null state.groups)
                        $>  HH.option
                              [ HP.value "group" ]
                              [ HH.text "Group" ]
                       )
                     ⊕ [ HH.option
                         [ HP.value "token" ]
                         [ HH.text "Token" ]
                       ]
                 ]
             , HH.label
                 [ HP.classes
                     $ (if state.subjectType ≠ User then [ B.hidden ] else [ ])
                     ⊕ (if state.error ≡ Just Validation then [ B.hasError ] else [ ])
                 , HP.for "user-email"
                 ]
                 [ HH.text "User email"
                 , HH.input
                     [ HP.classes [ B.formControl ]
                     , HP.id_ "user-email"
                     , HP.value state.email
                     , HE.onValueInput $ HE.input EmailChanged
                     , HP.placeholder "User email"
                     , HP.inputType HP.InputText
                     , HP.disabled state.submitting
                     ]
                 ]
              , HH.label
                  [ HP.classes $ if state.subjectType ≠ Group then [ B.hidden ] else [ ]
                  , HP.for "group-path"
                  ]
                  [ HH.text "Group path"
                  , HH.select
                      [ HP.classes [ B.formControl ]
                      , HP.id_ "group-path"
                      , HE.onValueChange (HE.input GroupSelected)
                      , HP.disabled $ Arr.null state.groups ∨ state.submitting
                      ]
                      -- I find using such things in where a bit complicated
                      -- because it's break context. OTOH, lambdas are not
                      -- so fancy as named func. cryogenian.
                      let
                        renderOption ∷ FilePath → HTML
                        renderOption group =
                          HH.option
                            [ HP.value $ Pt.printPath  group ]
                            [ HH.text $ Pt.printPath  group ]
                      in
                        map renderOption state.groups
                  ]
              , HH.label
                  [ HP.classes $ if state.subjectType ≠ Token then [ B.hidden ] else [ ]
                  , HP.for "token-name"
                  ]
                  [ HH.text "Token name"
                  , HH.input
                      [ HP.id_ "token-name"
                      , HP.classes [ B.formControl ]
                      , HP.inputType HP.InputText
                      , HP.placeholder "Token name"
                      , HP.value state.tokenName
                      , HE.onValueInput (HE.input TokenNameChanged)
                      , HP.disabled state.submitting
                      ]
                  ]
             , HH.label
                 [ HP.for "permission" ]
                 [ HH.text "Permission"
                 , HH.select
                     [ HP.id_ "permission"
                     , HP.classes [ B.formControl ]
                     , HE.onValueChange (HE.input (ChangeShareResume ∘ readShareResume))
                     , HP.disabled state.submitting
                     ]
                     [ HH.option
                         [ HP.value "view" ]
                         [ HH.text "View" ]
                     , HH.option
                         [ HP.value "edit" ]
                         [ HH.text "Edit" ]
                     ]
                 ]
             , HH.div
                  [ HP.classes
                      $ [ B.alert, B.alertDanger ]
                      ⊕ (if state.showError
                            ∧ (state.error ≡ Just Connection ∨ state.error ≡ Just GroupList)
                           then [ ]
                           else [ B.hidden ])
                  , HE.onClick (HE.input_ DismissError)
                  ]
                  [ HH.text
                      if state.error ≡ Just Connection
                      then
                        "This action couldn't be performed. "
                        ⊕ "Please check your network connection and try again"
                      else
                        "Groups are unavailable. To share this deck with a group "
                        ⊕ "please check your network connection and try again."
                  ]
             , HH.div
                 [ HP.classes
                     $ [ B.alert, B.alertInfo ]
                     ⊕ (if state.showError ∧ state.error ≡ Just Validation then [ ] else [ B.hidden ])
                 ]
                 [ HH.text "Please check if user email is correct" ]
             ]
         ]

    , HH.div
         [ HP.classes [ HH.className "deck-dialog-footer" ] ]
         $ [ HH.button
               [ HP.classes
                   $ [ B.btn, B.btnDefault ]
                   ⊕ (if state.loading then [ B.hidden ] else [ ])
               , HP.buttonType HP.ButtonButton
               , HE.onClick (HE.input_ Dismiss)
               , HP.disabled state.submitting
               ]
               [ HH.text "Dismiss" ]
           ]
           ⊕ (if isJust state.tokenSecret
                then [ ]
                else
                [ HH.button
                    [ HP.classes
                        $ [ B.btn, B.btnPrimary ]
                        ⊕ (if state.loading then [ B.hidden ] else [ ])
                        ⊕ (if isJust state.error && state.showError then [ B.hasError ] else [ ])
                    , HP.buttonType HP.ButtonButton
                    , HE.onClick (HE.input_ Share)
                    , HP.disabled (state.submitting ∨
                                   ((state.email ≡ "" ∨ state.error ≡ Just Validation)
                                    ∧ state.subjectType ≡ User))
                    ]
                    [ HH.text if state.submitting then "Sharing..." else  "Share" ]
                ])
    ]


eval ∷ Query ~> DSL
eval (Dismiss next) = pure next
eval (ChangeSubjectType st next) = do
  H.modify (_{subjectType = st})
  unless (st ≡ User)
    $ H.modify (_{ error = Nothing
                 , showError = false
                 })
  pure next
eval (Share next) = next <$ do
  H.modify (_{ error = Nothing
             , showError = false
             , submitting = true
             })

  H.modify (_{submitting = true})
  state ← H.get

  let
    actions = sharingActions state.sharingInput state.shareResume

  if state.subjectType ≡ Token
    then do
    res ←
      Q.createToken
        (if state.tokenName ≡ "" then Nothing else Just $ QT.TokenName state.tokenName)
        actions

    case res of
      Left _ →
        showConnectionError
      Right token →
        H.modify (_{ tokenSecret = Just $ QT.runTokenHash token.secret
                   , submitting = false
                   })
    else do
    let
      shareRequest ∷ QT.ShareRequestR
      shareRequest =
        { users: (if state.subjectType ≡ User
                  then [ QT.UserId state.email ]
                  else [ ])
        , groups: (if state.subjectType ≡ Group
                   then foldMap (pure ∘ Right) state.groupSelected
                   else [ ])
        , actions
        }
    res ← Q.sharePermission shareRequest
    case res of
      Left _ →
        showConnectionError
      Right [] →
        showConnectionError
      _ → raise $ H.action Dismiss
eval (DismissError next) =
  H.modify (_{showError = false}) $> next
eval (EmailChanged str next) = do
  let
    incorrectEmail = emailIsIncorrect str
  H.modify (_{email = str})
  mbError ← H.gets _.error
  unless (mbError ≡ Just Connection)
    $ H.modify
      if incorrectEmail
        then (_{ error = Just Validation
               , showError = true
               })
        else (_{ error = Nothing
               , showError = false
               })
  pure next
eval (Init next) = next <$ do
  res ← Q.groupInfo (rootDir </> file "")
  case res of
    Left _ →
      H.modify _{ error = Just GroupList
                , showError = true
                }
    Right grInfo →
      H.modify (_{groups = [rootFile] ⊕ grInfo.subGroups })
  H.modify (_{loading = false})
eval (GroupSelected grpString next) = next <$ do
  groups ← H.gets _.groups
  let
    group =
      Arr.findIndex (\x → grpString ≡ Pt.printPath x) groups
      >>= Arr.index groups
  H.modify (_{groupSelected = group})
eval (TokenNameChanged str next) =
  H.modify (_{tokenName = str}) $> next
eval (ChangeShareResume sr next) =
  H.modify (_{shareResume = sr}) $> next
eval (InitZClipboard token mbEl next) =
  next <$ for_ mbEl \el → do
    H.fromEff $ Z.make (htmlElementToElement el)
      >>= Z.onCopy (Z.setData "text/plain" token)
eval (SelectElement el next) =
  next <$ H.fromEff (select el)

emailIsIncorrect ∷ String → Boolean
emailIsIncorrect = not ∘ Str.contains (Str.Pattern "@")

showConnectionError ∷ DSL Unit
showConnectionError =
  H.modify (_{ error = Just Connection
             , showError = true
             , submitting = false
             })

