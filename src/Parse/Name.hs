module Parse.Name where

import           AST.Source
import           Parse.Internal

import           Text.Megaparsec


exprReserved :: [String]
exprReserved = ["let", "in", "type", "over", "satisfy"]

makeIdentifier :: [String] -> Parser String
makeIdentifier rws = (lexeme . try) (ident >>= check)
  where
    check x | x `elem` rws = fail $ "attempt to parse " ++ show x ++ " as an identifier"
            | otherwise = return x

name :: Parser Name
name = makeIdentifier exprReserved

typeName :: Parser TypeName
typeName = ident

tvarName :: Parser TVarName
tvarName = symbol "'" >> ident
