{-# LANGUAGE TemplateHaskell #-}
module Config where

import           Control.Lens.TH
import qualified Data.Map        as Map


data LiteralTypes
  = LiteralTypes { integer :: String
                 , real    :: String
                 , char    :: String
                 , bool    :: String
                 , string  :: String }

data Config
  = Config { _types        :: Map.Map String String
           , _literalTypes :: LiteralTypes
           , _bindings     :: Map.Map String String }

makeLenses ''Config


loadDefaultConfigFile :: IO Config
loadDefaultConfigFile = return $ Config (Map.fromList [("Int", "*"), ("Char", "*"), ("Float", "*"), ("String", "*"), ("Bool", "*")])
                                 (LiteralTypes "Int" "Float" "Char" "Bool" "String")
                                 (Map.fromList [("eqInt", "Int -> Int -> Bool"), ("eqChar", "Char -> Char -> Bool")])
