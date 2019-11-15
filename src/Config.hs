{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Config where

import           AST.Kind
import           AST.Literal
import           AST.Type         hiding (type_)
import qualified Parse.Internal   as P (Parser)
import           Parse.Kind       (kind)
import           Parse.Type       (typeScheme, type_)
import           Reporting.Error

import           Data.Aeson.Types (typeMismatch)
import qualified Data.Array       as Array
import           Data.Bifunctor
import qualified Data.Map         as Map
import           Data.Text
import           Data.Yaml        hiding (Bool)
import           Text.Megaparsec  (eof, errorBundlePretty, parse)


data Config
  = Config { baseTypes    :: Map.Map String Kind
           , literalTypes :: MapLit Type
           , bindings     :: Map.Map String TypeScheme }


instance FromJSON Type where
  parseJSON (String text) = parseAndBundle type_ text
  parseJSON o             = typeMismatch "String" o

instance FromJSON TypeScheme where
  parseJSON (String text) = parseAndBundle typeScheme text
  parseJSON o             = typeMismatch "String" o

instance FromJSON Kind where
  parseJSON (String text) = parseAndBundle kind text
  parseJSON o             = typeMismatch "String" o

instance FromJSON (MapLit Type) where
  parseJSON (Object v) = mapM (v .:) ary
    where
      ary = Array.array (minBound, maxBound)
        [ (litK Int, "integer")
        , (litK Char, "char")
        , (litK Str, "string")
        , (litK Real, "real")
        , (litK Bool, "boolean") ]
  parseJSON o = typeMismatch "Object" o

instance FromJSON Config where
  parseJSON (Object v) = Config
    <$> v .: "base_types"
    <*> v .: "literal_types"
    <*> v .: "bindings"
  parseJSON o = typeMismatch "Object" o


parseAndBundle :: P.Parser a -> Text -> Parser a
parseAndBundle p input =
  case parse (p <* eof) "" input of
    Left err -> fail $ errorBundlePretty err
    Right s  -> return s


loadConfigFile :: String -> IO (Either Error Config)
loadConfigFile path = first (ConfigError . prettyPrintParseException) <$> decodeFileEither path

loadDefaultConfigFile :: IO (Either Error Config)
loadDefaultConfigFile = loadConfigFile "env.yaml"
