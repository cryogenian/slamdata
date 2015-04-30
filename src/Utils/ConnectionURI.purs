module Utils.ConnectionURI
  ( URIParams()
  , CredentialParams()
  , HostParams()
  , PropParams()
  , parse
  , toURI
  ) where

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Data.Array (null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.String (joinWith, charAt, fromChar)
import Text.Parsing.StringParser (Parser(), ParseError(), runParser)
import Text.Parsing.StringParser.Combinators (many1, sepBy, sepBy1, optionMaybe, manyTill, lookAhead)
import Text.Parsing.StringParser.String (string, anyChar, anyDigit, eof)

type URIParams =
  { name :: Maybe String
  , credentials :: Maybe CredentialParams
  , hosts :: Array HostParams
  , props :: Array PropParams
  }

type CredentialParams = { user :: String, password :: String }
type HostParams = { host :: String, port :: Maybe String }
type PropParams = { name :: String, value :: String }

toURI :: URIParams -> String
toURI params = "mongodb://"
  ++ maybe "" credentialsToURI params.credentials
  ++ (joinWith "," $ hostToURI <$> params.hosts)
  ++ "/" ++ (fromMaybe "" params.name)
  ++ if null params.props
     then ""
     else "?" ++ (joinWith "&" $ propToURI <$> params.props)
  where
  credentialsToURI :: CredentialParams -> String
  credentialsToURI cred = cred.user ++ ":" ++ cred.password ++ "@"
  hostToURI :: HostParams -> String
  hostToURI host = host.host ++ (maybe "" (":" ++) host.port)
  propToURI :: PropParams -> String
  propToURI prop = prop.name ++ "=" ++ prop.value

parse :: String -> Either ParseError URIParams
parse = runParser parseURI

parseURI :: Parser URIParams
parseURI = do
  string "mongodb://"
  credentials <- optionMaybe parseCredentials
  hosts <- sepBy1 parseHost (string ",")
  name <- optionMaybe parseDatabaseName
  options <- sepBy parseProp parsePropSep
  eof
  return { name: name
         , credentials: credentials
         , hosts: hosts
         , props: options
         }

parseCredentials :: Parser { user :: String, password :: String }
parseCredentials = lookAhead (string "@") *> ({ user: _, password: _ }
  <$> (joinWith "" <$> manyTill anyChar (string ":"))
  <*> (joinWith "" <$> manyTill anyChar (string "@")))

parseHost :: Parser { host :: String, port :: Maybe String }
parseHost = { host: _, port: _ }
  <$> (joinWith "" <$> manyTill anyChar (string ":" <|> (lookAhead (string "/" <|> string ","))))
  <*> (optionMaybe $ joinWith "" <$> many1 anyDigit)

parseDatabaseName :: Parser String
parseDatabaseName = do
  string "/"
  joinWith "" <$> manyTill anyChar ((string "?" *> pure unit) <|> eof)

parseProp :: Parser { name :: String, value :: String }
parseProp = { name: _, value: _ }
  <$> (joinWith "" <$> manyTill anyChar (string "="))
  <*> (joinWith "" <$> manyTill anyChar (lookAhead parsePropSep <|> eof))

parsePropSep :: Parser Unit
parsePropSep = (string "&" <|> string ";") *> pure unit
