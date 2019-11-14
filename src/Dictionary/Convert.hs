{-# LANGUAGE FlexibleContexts #-}
module Dictionary.Convert where

import qualified AST.Intermediate          as T
import qualified AST.Source                as S
import           Dictionary.Env

import           Control.Eff
import           Control.Eff.Reader.Strict
import           Control.Monad             ((<=<))
import           Data.Functor.Foldable


runConvert :: S.Expr -> T.Expr
runConvert = run . runReader initEnv . convert

convert :: Member (Reader Env) r => S.Expr -> Eff r T.Expr
convert = cataM go
  where
    go (S.IntF i)       = return $ T.Int i
    go (S.CharF c)      = return $ T.Char c
    go (S.StrF s)       = return $ T.Str s
    go (S.RealF f)      = return $ T.Real f
    go (S.BoolF b)      = return $ T.Bool b
    go (S.VarF x)       = return $ T.Var x
    go (S.AppF a b)     = return $ T.App a b
    go (S.LamF x body)  = return $ T.Lam x body
    go (S.TupleF xs)    = return $ T.Tuple xs
    go (S.LetF x e1 e2) = return $ T.Let x e1 e2
    go (S.ClassF cls e) = undefined
    go (S.ImplF impl e) = undefined


cataM
  :: (Monad m, Traversable (Base t), Recursive t)
  => (Base t a -> m a) -> t -> m a
cataM alg = c where
  c = alg <=< traverse c . project
