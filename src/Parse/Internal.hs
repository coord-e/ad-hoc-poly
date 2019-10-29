module Parse.Internal where

import           Data.Functor
import           Data.Void
import           Text.Megaparsec
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String


space :: Parser ()
space = L.space C.space1 empty block
  where
    block = L.skipBlockComment "(*" "*)"

symbol :: String -> Parser ()
symbol = void . L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

ident :: Parser String
ident = lexeme $ (:) <$> C.letterChar <*> many C.alphaNumChar

rword :: String -> Parser ()
rword w = (lexeme . try) (C.string w *> notFollowedBy C.alphaNumChar)
