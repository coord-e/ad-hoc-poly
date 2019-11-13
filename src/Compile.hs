module Compile where

import           Config           (Config, loadConfigFile,
                                   loadDefaultConfigFile)
import qualified Emit             (emit)
import qualified Overload         (compile)
import qualified Parse            (parseIntermediate)
import           Reporting.Result (Result)

import           Control.Monad    ((<=<))
import           Data.Text
import qualified Data.Text.IO     as T


compileFile :: Maybe FilePath -> FilePath -> IO (Result String)
compileFile mconfig file = do
  config <- maybe loadDefaultConfigFile loadConfigFile mconfig
  content <- T.readFile file
  return $ flip compile content =<< config

compile :: Config -> Text -> Result String
compile c = fmap Emit.emit . Overload.compile c <=< Parse.parseIntermediate
