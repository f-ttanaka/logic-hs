module ToyProlog.Core.Unify where

import qualified Data.Map as M
import ToyProlog.Common
import ToyProlog.Data.Term

data Subst = Subst (Map Var Term)
  deriving (Show)

instance Semigroup Subst where
  Subst s1 <> s2@(Subst m2) = Subst $ (fmap (apply s2) s1) <> m2

instance Monoid Subst where
  mempty = Subst mempty

class Substitutable a where
  apply :: Subst -> a -> a

instance Substitutable Term where
  apply (Subst s) tx@(TVar x)
    | Just t <- M.lookup x s = t
    | otherwise = tx
  apply s (TFunc f ts) = TFunc f (map (apply s) ts)

unify :: Term -> Term -> Maybe Subst
unify t1 t2 = unifies mempty [(t1, t2)]

unifies :: Subst -> [(Term, Term)] -> Maybe Subst
unifies s [] = Just s
unifies s (e : es) = case e of
  (TVar x, TVar y) | x == y -> unifies s es
  (TVar x, t)
    | not (x `occursIn` t) ->
        let sigma = Subst $ M.singleton x t
            es' = [(apply sigma t1, apply sigma t2) | (t1, t2) <- es]
         in unifies (sigma <> s) es'
  (t, tv@(TVar _)) -> unifies s ((tv, t) : es)
  (TFunc f1 ts1, TFunc f2 ts2)
    | f1 == f2 && length ts1 == length ts2 ->
        unifies s $ (zip ts1 ts2) ++ es
  _ -> Nothing

occursIn :: Var -> Term -> Bool
occursIn x (TVar y) = x == y
occursIn x (TFunc _ ts) = any (occursIn x) ts
