{-# LANGUAGE OverloadedStrings #-}
module Parse.Internal where

import           Data.Functor
import           Data.Text                  hiding (empty)
import           Data.Void
import           Text.Megaparsec
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text


space :: Parser ()
space = L.space C.space1 empty block
  where
    block = L.skipBlockComment "(*" "*)"

symbol :: Text -> Parser ()
symbol = void . L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

ident :: Parser String
ident = lexeme $ (:) <$> C.letterChar <*> many C.alphaNumChar

rword :: Text -> Parser ()
rword w = (lexeme . try) (C.string w *> notFollowedBy C.alphaNumChar)
