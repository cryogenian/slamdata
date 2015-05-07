module Api.Fs where

import Control.Monad.Aff(Aff())
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), either)
import Data.Maybe 

import Data.Array (head)
import Data.String (split)
import Data.Foreign.Class (readProp, read, IsForeign)
import Data.Int (fromNumber, toNumber)
import Network.HTTP.Affjax.Response (Respondable, ResponseType(JSONResponse))
import Network.HTTP.StatusCode (StatusCode(..))
import Network.HTTP.Affjax (Affjax(), AJAX(), affjax, get, put_, delete_, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.Method (Method(MOVE))
import Data.Argonaut.Parser (jsonParser)

import Model.Notebook (Notebook())
import Model.Resource
import Config
import Data.Path.Pathy
import Optic.Core

newtype Listing = Listing [Resource]

runListing :: Listing -> [Resource]
runListing (Listing rs) = rs 

instance listingIsForeign :: IsForeign Listing where
  read f = Listing <$> readProp "children" f 

instance listingRespondable :: Respondable Listing where
  responseType = JSONResponse
  fromResponse = read 

successStatus :: StatusCode
successStatus = StatusCode $ fromNumber 200

succeeded :: StatusCode -> Boolean
succeeded (StatusCode int) =
  200 <= code && code < 300
  where code = toNumber int

getResponse :: forall a e. String -> Affjax e a -> Aff (ajax :: AJAX | e) a 
getResponse msg affjax = do 
  res <- affjax
  if not $ succeeded res.status
    then throwError $ error msg
    else pure res.response

children :: forall e. Resource -> Aff (ajax :: AJAX | e) [Resource] 
children r = do
  cs <- children' $ resourcePath r 
  pure $ (\x -> x # rootL .~ (either (const rootDir) id $ getPath r)) <$> cs 
  where
  msg = "error getting resource children"
  
  children' :: String -> Aff _ [Resource]
  children' str = runListing <$> (getResponse msg $ listing str)
  
  listing :: String -> Affjax _ Listing
  listing str = get (Config.metadataUrl <> str)

makeFile :: forall e. AnyPath -> String -> Aff (ajax :: AJAX | e) Unit 
makeFile ap content = 
  getResponse msg $ either err go isJson
  where
  resource :: Resource
  resource = setPath newFile ap

  msg :: String 
  msg = "error while creating file"
  
  err :: _ -> Aff _ _
  err _ = throwError $ error "file has incorrect format" 
    
  firstLine :: Maybe String 
  firstLine = head $ split "\n" content

  isJson :: Either _ _
  isJson = maybe (Left "empty file") Right firstLine >>= jsonParser

  go :: _ -> Aff _ _
  go _ = put_ (Config.dataUrl <> resourcePath resource) content
    

makeNotebook :: forall e. AnyPath -> Notebook -> Aff (ajax :: AJAX | e) Unit 
makeNotebook ap notebook = 
  getResponse msg $ put_ (Config.dataUrl <> resourcePath resource) notebook
  where msg = "error while creating notebook"
        resource = setPath newNotebook ap

delete :: forall e. Resource -> Aff (ajax :: AJAX | e) Unit 
delete resource = 
  getResponse msg $ delete_ (Config.dataUrl <> resourcePath resource) 
  where msg = "can not delete" 


move :: forall e. Resource -> AnyPath -> Aff (ajax :: AJAX | e) String 
move src tgt = do
  result <- affjax $ defaultRequest 
    { method = MOVE
    , headers = [RequestHeader "Destination" $ resourcePath (setPath src tgt)]
    , url = Config.dataUrl <> resourcePath src 
    }
  pure if succeeded result.status
       then ""
       else result.response 
