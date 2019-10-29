module Parse.Kind where

import           AST.Source
import           Parse.Internal

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec


term :: Parser Kind
term = try (parens kind)
  <|> (symbol "*" >> pure Star)
  <|> (symbol "C" >> pure Constraint)

table :: [[Operator Parser Kind]]
table =
  [ [ InfixR (Arrow <$ symbol "->") ] ]

kind :: Parser Kind
kind = makeExprParser term table
