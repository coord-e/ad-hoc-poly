{-# LANGUAGE OverloadedStrings #-}
module Parse.Internal where

import           Data.Char                  (isAlphaNum)
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
ident = lexeme $ (:) <$> C.letterChar <*> many (satisfy isIdentChar)
  where
    isIdentChar '_'  = True
    isIdentChar '\'' = True
    isIdentChar c    = isAlphaNum c

rword :: Text -> Parser Text
rword w = (lexeme . try) (C.string w <* notFollowedBy C.alphaNumChar)

rword_ :: Text -> Parser ()
rword_ = void . rword
