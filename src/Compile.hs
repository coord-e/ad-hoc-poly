module Compile where

import qualified AST.Intermediate as I
import           Config           (Config, loadConfigFile,
                                   loadDefaultConfigFile)
import qualified Dictionary       (compile)
import qualified Emit             (emit)
import qualified Overload         (compile)
import qualified Parse            (parseIntermediate, parseSource)
import           Reporting.Result (Result)

import           Control.Monad    (join, (<=<))
import           Data.Text
import qualified Data.Text.IO     as T


compileSourceFile :: Maybe FilePath -> FilePath -> IO (Result String)
compileSourceFile = compileFile (Dictionary.compile <=< Parse.parseSource)

compileIntermediateFile :: Maybe FilePath -> FilePath -> IO (Result String)
compileIntermediateFile = compileFile Parse.parseIntermediate

compileFile :: (Text -> Result I.Expr) -> Maybe FilePath -> FilePath -> IO (Result String)
compileFile f mconfig file = do
  config <- maybe loadDefaultConfigFile loadConfigFile mconfig
  content <- T.readFile file
  return . join $ compile <$> config <*> f content

compile :: Config -> I.Expr -> Result String
compile c = fmap Emit.emit . Overload.compile c
