module Unify where

import Common
import Data.List (lookup)

data Term
  = TInt Int
  | TNil
  | TCons Term Term
  | TVar Var
  deriving (Eq)

data Var
  = VNamed String
  | VGenerated Int
  deriving (Eq)

var :: String -> Term
var = TVar . VNamed

list :: [Int] -> Term
list xs = foldr TCons TNil (map TInt xs)

newtype Subst = MkSubst {unSubst :: [(Var, Term)]}

idSubst :: Subst
idSubst = MkSubst []

extend :: Var -> Term -> Subst -> Subst
extend x t (MkSubst s) = MkSubst $ (x, t) : s

apply :: Subst -> Term -> Term
apply s t = case deref s t of
  TCons x xs -> TCons (apply s x) (apply s xs)
  t' -> t'

deref :: Subst -> Term -> Term
deref s tv@(TVar v) = case lookup v (unSubst s) of
  Just t -> deref s t
  _ -> tv
deref _ t = t

unify :: (Term, Term) -> Subst -> Maybe Subst
unify (t, u) s = case (deref s t, deref s u) of
  (TNil, TNil) -> Just s
  (TCons x xs, TCons y ys) -> unify (xs, ys) =<< unify (x, y) s
  (TInt n, TInt m) | n == m -> Just s
  (TVar x, TVar y) | x == y -> Just s
  (TVar x, t) -> if occurs x t s then Nothing else Just (extend x t s)
  (t, TVar x) -> if occurs x t s then Nothing else Just (extend x t s)
  _ -> Nothing

occurs :: Var -> Term -> Subst -> Bool
occurs x t s = case deref s t of
  TVar y -> x == y
  TCons y ys -> occurs x y s || occurs x ys s
  _ -> False
