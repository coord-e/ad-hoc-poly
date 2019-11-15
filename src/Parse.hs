module Parse where

import qualified AST.Intermediate   as IAST
import qualified AST.Source         as SAST
import qualified Parse.Intermediate as I
import           Parse.Internal     (Parser)
import qualified Parse.Source       as S
import           Reporting.Error
import           Reporting.Result

import           Data.Bifunctor
import           Data.Text
import qualified Text.Megaparsec    as M


parseIntermediate :: Text -> Result IAST.Expr
parseIntermediate = parseAndBundle I.expr

parseSource :: Text -> Result SAST.Expr
parseSource = parseAndBundle S.expr

parseAndBundle :: Parser a -> Text -> Result a
parseAndBundle p = first (ParseError . M.errorBundlePretty) . M.parse (p <* M.eof) "input"
