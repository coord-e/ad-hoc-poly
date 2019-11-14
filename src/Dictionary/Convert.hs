{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Dictionary.Convert where

import qualified AST.Intermediate           as T
import qualified AST.Source                 as S
import qualified AST.Type                   as T
import           Dictionary.Env
import           Reporting.Error
import           Reporting.Error.Dictionary
import           Reporting.Result

import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.Reader.Strict
import           Control.Lens.Indexed       (ifoldr)
import           Control.Monad.Extra        (fromMaybeM)
import           Data.Bifunctor
import qualified Data.Map                   as Map


runConvert :: S.Expr -> Result T.Expr
runConvert = run . runError . runReader initEnv . convert

convert :: (Member (Exc Error) r, Member (Reader Env) r) => S.Expr -> Eff r T.Expr
convert (S.Int i)       = return $ T.Int i
convert (S.Char c)      = return $ T.Char c
convert (S.Str s)       = return $ T.Str s
convert (S.Real f)      = return $ T.Real f
convert (S.Bool b)      = return $ T.Bool b
convert (S.Var x)       = return $ T.Var x
convert (S.App a b)     = T.App <$> convert a <*> convert b
convert (S.Lam x body)  = T.Lam x <$> convert body
convert (S.Tuple xs)    = T.Tuple <$> mapM convert xs
convert (S.Let x e1 e2) = T.Let x <$> convert e1 <*> convert e2
convert (S.Class cls e) = convertClass cls $ convert e
convert (S.Impl impl e) = convertImpl impl =<< convert e


convertClass :: Member (Reader Env) r => S.ClassDecl -> Eff r T.Expr -> Eff r T.Expr
convertClass (S.ClassDecl as cls cs ms) m = eType <$> local (Map.insert cls names) m
  where
    dName = "d" ++ cls
    tName = cls
    len = length ms
    (names, dTuple) = second T.TTuple $ unzip ms
    tConstraint = T.TConstraint dName dTuple
    tPredicated = foldr (\(t, c) -> T.TPredicate (T.TApp (T.TName c) t)) tConstraint cs
    tCon = foldr T.TLam tPredicated as
    tScheme = T.Forall as (foldl (\acc -> T.TApp acc . T.TVar) (T.TName tName) as)
    eType = T.Type tName tCon . eOver
    eOver = T.Over tScheme . eLet
    eLet e = ifoldr (\i x -> T.Let x (T.Nth len i (T.Var dName))) e names

convertImpl :: (Member (Exc Error) r, Member (Reader Env) r) => S.ImplDecl -> T.Expr -> Eff r T.Expr
convertImpl (S.ImplDecl as cls tgt cs ms) e = do
  impls <- Map.fromList <$> mapM (secondM convert) ms
  defs <- fromMaybeM (throwError . DictionaryError $ UndeclaredClass cls) $ reader (Map.lookup cls)
  tSatisfy . T.Tuple <$> buildTuple impls defs
  where
    tName = cls
    tConstraint = foldl T.TApp (T.TName tName) tgt
    tPredicated = foldr (\(t, c) -> T.TPredicate (T.TApp (T.TName c) t)) tConstraint cs
    tScheme = T.Forall as tPredicated
    tSatisfy tup = T.Satisfy tScheme tup e
    buildTuple = mapM . findImpl
    findImpl impls k = maybe (throwError . DictionaryError $ MissingImpl cls k) pure $ Map.lookup k impls


secondM :: Functor f => (b -> f c) -> (a, b) -> f (a, c)
secondM f (x, y) = (x,) <$> f y
