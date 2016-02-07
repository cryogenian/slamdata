module Test.Selenium.Errors where

-- Errors
throwLocatorError :: forall a. String -> Locator -> Check a
throwLocatorError errorPartial =
  throwString <<< concatinateWithErrorPartial <<< showLocator
  where
  throwString = throwError <<< error
  concatinateWithErrorPartial locator = errorPartial ++ "using the locator: " ++ locator ++ "."

throwNoElementWithPropertyError :: forall a. Locator -> (Maybe String) -> String -> Check a
throwNoElementWithPropertyError name value =
  throwLocatorError message
  where
  stringValue = maybe "null" show value
  message =
    "Unable to find element with "
      ++ show name
      ++ " attribute or property of "
      ++ show stringValue

