module Unify where

import Common
import Data.List (lookup)

type Var = String

type Symbol = String

data Term
  = TVar Var
  | TAtom String
  | TFunc Symbol [Term]
  deriving (Show, Eq)

data Rule = Rule Term [Term]

type Query = [Term]

data Subst = Subst [(Var, Term)]
  deriving (Show)

instance Semigroup Subst where
  Subst s1 <> Subst s2 = Subst $ s1 ++ s2

instance Monoid Subst where
  mempty = Subst []

unify :: Term -> Term -> Maybe Subst
unify (TVar v) t = Just $ Subst [(v, t)]
unify t (TVar v) = Just $ Subst [(v, t)]
unify (TAtom a1) (TAtom a2)
  | a1 == a2 = Just mempty
  | otherwise = Nothing
unify (TFunc f1 ts1) (TFunc f2 ts2)
  | f1 == f2 && length ts1 == length ts2 = unifyArgs ts1 ts2
  | otherwise = Nothing
unify _ _ = Nothing

unifyArgs :: [Term] -> [Term] -> Maybe Subst
unifyArgs [] [] = Just mempty
unifyArgs (t1 : ts1) (t2 : ts2) = do
  s1 <- unify t1 t2
  s2 <- unifyArgs (map (apply s1) ts1) (map (apply s1) ts2)
  Just $ s1 <> s2
unifyArgs _ _ = Nothing

apply :: Subst -> Term -> Term
apply (Subst s) tx@(TVar x)
  | Just t <- lookup x s = t
  | otherwise = tx
apply s (TFunc f ts) = TFunc f (map (apply s) ts)
apply _ t = t

-- resolve substs by backtracking
resolve :: [Rule] -> Query -> [Subst]
resolve _ [] = [mempty]
resolve rs (goal : rest) = do
  Rule h b <- rs
  s <- maybeToList (unify goal h)
  let newGoals = map (apply s) (b <> rest)
  s' <- resolve rs newGoals
  return $ s <> s'

(^-) :: Term -> [Term] -> Rule
h ^- b = Rule h b

rules =
  [ TFunc "parent" [TVar "X", TVar "Y"] ^- [TFunc "mother" [TVar "X", TVar "Y"]],
    TFunc "mother" [TAtom "alice", TAtom "bob"] ^- []
  ]

query = [TFunc "parent" [TAtom "alice", TVar "Y"]]
