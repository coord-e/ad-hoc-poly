{-# LANGUAGE OverloadedStrings #-}
module Parse.Expr where

import           AST.Source                     hiding (type_)
import           Parse.Internal
import           Parse.Name
import           Parse.Type

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec                hiding (satisfy)
import qualified Text.Megaparsec.Char.Lexer     as L


integer :: Parser Int
integer = lexeme L.decimal

real :: Parser Double
real = try $ lexeme L.float

bool :: Parser Bool
bool = (== "true") <$> choice [ rword "true", rword "false" ]

quotedChar :: Parser Char
quotedChar = between (symbol "'") (symbol "'") L.charLiteral

quotedString :: Parser String
quotedString = symbol "\"" >> manyTill L.charLiteral (symbol "\"")

term :: Parser Expr
term = try (parens expr)
  <|> tuple
  <|> let_
  <|> typedef
  <|> over
  <|> satisfy
  <|> lambda
  <|> Bool <$> bool
  <|> Var <$> name
  <|> Real <$> real
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
