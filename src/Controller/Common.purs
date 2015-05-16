module Controller.Common where

import Api.Fs (children)
import Control.Monad.Aff (attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Plus (empty)
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Halogen.HTML.Events.Monad (Event(), andThen)
import Model.Resource
import Network.HTTP.Affjax (AJAX())

getChildren :: forall i e. (Resource -> Boolean) -> ([Resource] -> Event (ajax :: AJAX | e) i) -> Resource -> Event (ajax :: AJAX | e) i
getChildren pred f r = do
  ei <- liftAff $ attempt $ children r
  case ei of
    Right items -> do
      let items' = filter pred items
          parents = filter (\x -> isDirectory x || isDatabase x) items
      f items' `andThen` \_ -> fold (getChildren pred f <$> parents)
    _ -> empty

getDirectories :: forall i e. ([Resource] -> Event (ajax :: AJAX | e) i) -> Resource -> Event (ajax :: AJAX | e) i
getDirectories = getChildren (\x -> isDirectory x || isDatabase x)

getFiles :: forall i e. ([Resource] -> Event (ajax :: AJAX | e) i) -> Resource -> Event (ajax :: AJAX | e) i
getFiles = getChildren isFile
