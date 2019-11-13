module Parse where

import qualified AST.Intermediate   as IAST
import qualified AST.Source         as SAST
import qualified Parse.Intermediate as I
import qualified Parse.Source       as S
import           Reporting.Error
import           Reporting.Result

import           Data.Bifunctor
import           Data.Text
import qualified Text.Megaparsec    as M


parseIntermediate :: Text -> Result IAST.Expr
parseIntermediate = first (ParseError . M.errorBundlePretty) . M.parse I.expr "input"

parseSource :: Text -> Result SAST.Expr
parseSource = first (ParseError . M.errorBundlePretty) . M.parse S.expr "input"
