{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module AST.Source where

import           AST.Name
import           AST.Type
import           Reporting.Report

import           Control.Lens.TH
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.List                (intercalate)


data ClassDecl
  = ClassDecl { _cdIntros     :: [TypeName]
              , _cdClass      :: TypeName
              , _cdPredicates :: [(Type, TypeName)]
              , _cdMethods    :: [(Name, Type)] }
              deriving (Show, Eq)


data ImplDecl
  = ImplDecl { _idIntros     :: [TypeName]
             , _idClass      :: TypeName
             , _idTypes      :: [Type]
             , _idPredicates :: [(Type, TypeName)]
             , _idMethods    :: [(Name, Expr)] }
             deriving (Show, Eq)


data Expr
  = Int Int
  | Char Char
  | Str String
  | Real Double
  | Bool Bool
  | Var Name
  | App Expr Expr
  | Lam Name Expr
  | Tuple [Expr]
  | Let Name Expr Expr
  | Class ClassDecl Expr
  | Impl ImplDecl Expr
  deriving (Show, Eq)


makeLenses ''ClassDecl
makeLenses ''ImplDecl
makeBaseFunctor ''Expr


-- Report instances
reportVars :: [TypeName] -> String
reportVars  = intercalate ", "

reportPreds :: [(Type, TypeName)] -> String
reportPreds = intercalate ", " . map (\(t, n) -> report t ++ ": " ++ n)

instance Report ClassDecl where
  report (ClassDecl as cls cs ms) = "class<" ++ reportVars as ++ "> " ++ cls
                                  ++ " where " ++ reportPreds cs ++ " {\n" ++ ms' ++ "}"
    where
      ms' = intercalate ",\n" $ map (\(n, t) -> n ++ " :: " ++ report t) ms

instance Report ImplDecl where
  report (ImplDecl as cls tgt cs ms) = "impl<" ++ reportVars as ++ "> " ++ cls ++ "<" ++ tgt' ++ ">"
                                     ++ " where " ++ reportPreds cs ++ " {\n" ++ ms' ++ "}"
    where
      ms' = intercalate ",\n" $ map (\(n, me) -> n ++ " = " ++ report me) ms
      tgt' = intercalate ", " $ map report tgt

instance Report Expr where
  report = cata go
    where
      go (IntF i)       = show i
      go (CharF c)      = show c
      go (StrF s)       = show s
      go (RealF f)      = show f
      go (BoolF b)      = show b
      go (VarF x)       = x
      go (AppF e1 e2)   = paren (e1 ++ " " ++ e2)
      go (LamF x e)     = paren ("λ" ++ x ++ ". " ++ e)
      go (TupleF xs)    = paren (intercalate ", " xs)
      go (LetF x e1 e2) = "let " ++ x ++ " = " ++ e1 ++ " in\n" ++ e2
      go (ClassF cls e) = report cls ++ " in\n" ++ e
      go (ImplF impl e) = report impl ++ " in\n" ++ e
