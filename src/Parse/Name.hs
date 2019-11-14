{-# LANGUAGE OverloadedStrings #-}
module Parse.Name where

import           AST.Name
import           Parse.Internal

import           Text.Megaparsec


exprReserved :: [String]
exprReserved = ["let", "in", "type", "over", "satisfy", "true", "false"]

typeReserved :: [String]
typeReserved = ["constraint", "in"]

makeIdentifier :: [String] -> Parser String
makeIdentifier rws = (lexeme . try) (ident >>= check)
  where
    check x | x `elem` rws = fail $ "attempt to parse " ++ show x ++ " as an identifier"
            | otherwise = return x

name :: Parser Name
name = makeIdentifier exprReserved

typeName :: Parser TypeName
typeName = makeIdentifier typeReserved

tvarName :: Parser TVarName
tvarName = symbol "'" >> typeName
