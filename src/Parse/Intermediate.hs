{-# LANGUAGE OverloadedStrings #-}
module Parse.Intermediate where

import           AST.Intermediate
import           Parse.Internal
import           Parse.Literal
import           Parse.Name
import           Parse.Type

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec                hiding (satisfy)


term :: Parser Expr
term = try (parens expr)
  <|> tuple
  <|> let_
  <|> typedef
  <|> over
  <|> satisfy
  <|> lambda
  <|> Lit <$> literal
  <|> Var <$> name

table :: [[Operator Parser Expr]]
table =
  [ [ InfixL (App <$ symbol "") ] ]

expr :: Parser Expr
expr = makeExprParser term table

tuple :: Parser Expr
tuple = Tuple <$> parens (expr `sepEndBy` symbol ",")

lambda :: Parser Expr
lambda = do
  symbol "λ" <|> symbol "\\"
  x <- name
  symbol "."
  Lam x <$> expr

let_ :: Parser Expr
let_ = do
  rword_ "let"
  x <- name
  symbol "="
  e1 <- expr
  rword_ "in"
  Let x e1 <$> expr

typedef :: Parser Expr
typedef = do
  rword_ "type"
  x <- typeName
  symbol "="
  t <- type_
  rword_ "in"
  Type x t <$> expr

over :: Parser Expr
over = do
  rword_ "overload"
  s <- typeScheme
  rword_ "in"
  Over s <$> expr

satisfy :: Parser Expr
satisfy = do
  rword_ "instance"
  s <- typeScheme
  symbol "="
  e <- expr
  rword_ "in"
  Satisfy s e <$> expr
