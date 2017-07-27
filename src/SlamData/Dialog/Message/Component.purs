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

module SlamData.Dialog.Message.Component where

import SlamData.Prelude

import Halogen as H
import SlamData.Dialog.Component as D
import SlamData.Render.ClassName as CN

type MessageQuery o = Tuple (D.Message o)

type MessageSpec o =
  { title ∷ String
  , message ∷ H.ComponentHTML (Const Void)
  , class_ ∷ H.ClassName
  , action ∷ Either String (String × o)
  }

mkSpec ∷ ∀ o m. MessageSpec o → D.DialogSpec o m
mkSpec { title, class_, action, message } =
  D.dialog
    $ D.withTitle title
    >>> D.withInitialState unit
    >>> D.withClass class_
    >>> D.withRender (const (absurd ∘ unwrap <$> message))
    >>> D.withEval eval
    >>> D.withButton
        (D.button
          $ D.withLabel (either id (const "Cancel") action)
          >>> D.withAction (const (Just (Tuple D.Dismiss))))
    >>> case action of
        Left _ → id
        Right (label × o) →
          D.withButton
            (D.button
              $ D.withLabel label
              >>> D.withClass CN.btnPrimary
              >>> D.withAction (const (Just (Tuple (D.Bubble o)))))

eval ∷ ∀ o m. MessageQuery o ~> H.ComponentDSL Unit (MessageQuery o) (D.Message o) m
eval (Tuple msg next) = do
  H.raise msg
  pure next
