{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Config where

import           AST.Source
import qualified Parse.Internal   as P (Parser)
import           Parse.Kind       (kind)
import           Parse.Type       (typeScheme)
import           Reporting.Error

import           Data.Aeson.Types (typeMismatch)
import           Data.Bifunctor
import qualified Data.Map         as Map
import           Data.Text
import           Data.Yaml
import           Text.Megaparsec  (errorBundlePretty, parse)


data LiteralTypes
  = LiteralTypes { integer :: TypeScheme
                 , real    :: TypeScheme
                 , char    :: TypeScheme
                 , boolean :: TypeScheme
                 , string  :: TypeScheme }

data Config
  = Config { baseTypes    :: Map.Map String Kind
           , literalTypes :: LiteralTypes
           , bindings     :: Map.Map String TypeScheme }


instance FromJSON TypeScheme where
  parseJSON (String text) = parseAndBundle typeScheme text
  parseJSON o             = typeMismatch "String" o

instance FromJSON Kind where
  parseJSON (String text) = parseAndBundle kind text
  parseJSON o             = typeMismatch "String" o

instance FromJSON LiteralTypes where
  parseJSON (Object v) = do
    integer <- v .: "integer"
    real <- v .: "real"
    char <- v .: "char"
    boolean <- v .: "boolean"
    string <- v .: "string"
    return $ LiteralTypes { integer, real, char, boolean, string }
  parseJSON o = typeMismatch "Object" o

instance FromJSON Config where
  parseJSON (Object v) = Config
    <$> v .: "base_types"
    <*> v .: "literal_types"
    <*> v .: "bindings"
  parseJSON o = typeMismatch "Object" o


parseAndBundle :: P.Parser a -> Text -> Parser a
parseAndBundle p input =
  case parse p "" input of
    Left err -> fail $ errorBundlePretty err
    Right s  -> return s


loadConfigFile :: String -> IO (Either Error Config)
loadConfigFile path = first (ConfigError . prettyPrintParseException) <$> decodeFileEither path

loadDefaultConfigFile :: IO (Either Error Config)
loadDefaultConfigFile = loadConfigFile "env.yaml"
