{-# LANGUAGE OverloadedStrings #-}
module Parse.Expr where

import           AST.Source                     hiding (type_)
import           Parse.Internal
import           Parse.Name
import           Parse.Type

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec                hiding (satisfy)
import qualified Text.Megaparsec.Char           as C
import qualified Text.Megaparsec.Char.Lexer     as L


integer :: Parser Int
integer = lexeme L.decimal

quotedChar :: Parser Char
quotedChar = between (symbol "'") (symbol "'") C.printChar

quotedString :: Parser String
quotedString = between (symbol "\"") (symbol "\"") $ many C.printChar

term :: Parser Expr
term = try (parens expr)
  <|> tuple
  <|> let_
  <|> typedef
  <|> over
  <|> satisfy
  <|> lambda
  <|> Var <$> name
  <|> Int <$> integer
  <|> Char <$> quotedChar
  <|> Str <$> quotedString

table :: [[Operator Parser Expr]]
table =
  [ [ InfixL (App <$ symbol "") ] ]

expr :: Parser Expr
expr = makeExprParser term table

tuple :: Parser Expr
tuple = Tuple <$> parens (expr `sepEndBy` symbol ",")

lambda :: Parser Expr
lambda = do
  symbol "λ"
  x <- name
  symbol "."
  Lam x <$> expr

let_ :: Parser Expr
let_ = do
  rword "let"
  x <- name
  symbol "="
  e1 <- expr
  rword "in"
  Let x e1 <$> expr

typedef :: Parser Expr
typedef = do
  rword "type"
  x <- typeName
  symbol "="
  t <- type_
  rword "in"
  Type x t <$> expr

over :: Parser Expr
over = do
  rword "over"
  s <- typeScheme
  rword "in"
  Over s <$> expr

satisfy :: Parser Expr
satisfy = do
  rword "satisfy"
  s <- typeScheme
  symbol "="
  e <- expr
  rword "in"
  Satisfy s e <$> expr
