{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Overload.TypeEval where

import qualified AST.Source                as S
import           Overload.Env
import           Overload.Type
import           Overload.Var

import           Control.Eff
import           Control.Eff.Reader.Strict
import           Control.Eff.State.Strict
import           Control.Lens
import           Control.Monad             (replicateM)
import           Control.Monad.Extra       (fromMaybeM)
import           Data.Functor.Foldable
import qualified Data.Map                  as Map


eval :: Member (Reader Env) r => S.Type -> Eff r S.Type
eval (S.TName x)               = reader $ flip (Map.!) x . view typeEnv
eval (S.TFun t1 t2)            = S.TFun <$> eval t1 <*> eval t2
eval (S.TTuple ts)             = S.TTuple <$> mapM eval ts
eval (S.TConstraint x t)       = S.TConstraint x <$> eval t
eval (S.TApp t1 t2) = do
  t1' <- eval t1
  t2' <- eval t2
  let (S.TLam x body) = t1'
  eval $ cata (subst x t2') body
eval t = return t

subst :: S.TypeName -> S.Type -> S.TypeF S.Type -> S.Type
subst n r (S.TNameF x) | x == n = r
subst _ _ t            = embed t


translateType :: Map.Map String TyVar -> S.Type -> Type
translateType m t = run $ runReader m (cata go t)
  where
    go S.TIntF         = return TInt
    go S.TCharF        = return TChar
    go S.TStrF         = return TStr
    go (S.TVarF n)     = TVar <$> fromMaybeM (error ("attempt to translate a type with unbound type variable " ++ n)) (reader (Map.lookup n))
    go (S.TFunF t1 t2) = TFun <$> t1 <*> t2
    go (S.TTupleF ts)  = TTuple <$> sequence ts
    go _               = error "attempt to translate non-* type"

translateConstraint :: Map.Map String TyVar -> S.Type -> Constraint
translateConstraint m (S.TConstraint x t) = Constraint x (translateType m t)
translateConstraint _ _ = error "attempt to translate non-Constraint type"

translateScheme :: Member (State Infer) r => S.TypeScheme -> Eff r TypeScheme
translateScheme (S.Forall vs cs t) = do
  as <- mapM (const fresh) vs
  let m = Map.fromList $ zip vs as
  return $ Forall as (map (translateConstraint m) cs) (translateType m t)
