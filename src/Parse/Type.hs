{-# LANGUAGE OverloadedStrings #-}
module Parse.Type (typeName, type_, typeScheme) where

import           AST.Source                     hiding (type_)
import           Parse.Internal
import           Parse.Name

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec


term :: Parser Type
term = try (parens type_)
  <|> tuple
  <|> lambda
  <|> constraint
  <|> TName <$> typeName
  <|> TVar <$> tvarName

table :: [[Operator Parser Type]]
table =
  [ [ InfixL (TApp <$ symbol "") ]
  , [ InfixR (TFun <$ symbol "->") ]
  , [ InfixR (TPredicate <$ symbol "=>") ] ]

type_ :: Parser Type
type_ = makeExprParser term table

tuple :: Parser Type
tuple = TTuple <$> parens (type_ `sepEndBy` symbol ",")

lambda :: Parser Type
lambda = do
  symbol "Λ"
  x <- typeName
  symbol "."
  TLam x <$> type_

constraint :: Parser Type
constraint = do
  rword "constraint"
  x <- name
  TConstraint x <$> typeScheme

typeSchemeBase :: Parser TypeScheme
typeSchemeBase = do
  xs <- qual <|> pure []
  Forall xs <$> type_
  where
    qual = do
      symbol "∀"
      xs <- some tvarName
      symbol "."
      return xs

typeScheme :: Parser TypeScheme
typeScheme = try (parens typeSchemeBase) <|> typeSchemeBase
