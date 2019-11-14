{-# LANGUAGE OverloadedStrings #-}
module Parse.Source where

import           AST.Name
import           AST.Source
import           AST.Type                       hiding (type_)
import           Parse.Internal
import           Parse.Name
import           Parse.Type

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec


term :: Parser Expr
term = try (parens expr)
  <|> tuple
  <|> let_
  <|> class_
  <|> impl
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

where_ :: Parser [(Type, TypeName)]
where_ = do
  rword_ "where"
  predi `sepBy` symbol ","
  where
    predi = do
      t <- type_
      symbol ":"
      c <- typeName
      return (t, c)

intros :: Parser [TypeName]
intros = between (symbol "<") (symbol ">") $ typeName `sepBy` symbol ","

class_ :: Parser Expr
class_ = do
  rword_ "class"
  as <- intros
  cls <- typeName
  cs <- where_ <|> pure []
  ms <- between (symbol "{") (symbol "}") $ method `sepEndBy1` symbol ","
  rword_ "in"
  Class (ClassDecl as cls cs ms) <$> expr
  where
    method = do
      n <- name
      symbol "::"
      t <- type_
      return (n, t)

impl :: Parser Expr
impl = do
  rword_ "impl"
  as <- intros <|> pure []
  cls <- typeName
  symbol "<"
  tgt <- type_ `sepBy1` symbol ","
  symbol ">"
  cs <- where_ <|> pure []
  ms <- between (symbol "{") (symbol "}") $ impl' `sepEndBy1` symbol ","
  rword_ "in"
  Impl (ImplDecl as cls tgt cs ms) <$> expr
  where
    impl' = do
      n <- name
      symbol "="
      e <- expr
      return (n, e)
