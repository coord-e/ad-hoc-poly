{-# LANGUAGE OverloadedStrings #-}
module Parse.Literal where

import           AST.Literal
import           Parse.Internal

import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L


literal :: Parser Literal
literal = choice
  [ Bool <$> bool
  , Real <$> real
  , Int <$> integer
  , Char <$> quotedChar
  , Str <$> quotedString ]


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
