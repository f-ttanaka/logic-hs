{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module ToyProlog.Core.Solve where

import Control.Exception.Safe
import ToyProlog.Common
import ToyProlog.Core.Unify
import ToyProlog.Data.Term

newtype Solve m a = Solve (StateT [Rule] m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState [Rule],
      MonadThrow
    )

solve :: (MonadThrow m) => Query -> Solve m [Subst]
solve [] = return [mempty]
solve (g : gs) = do

-- resolve substs by backtracking
-- resolve :: [Rule] -> Query -> [Subst]
-- resolve _ [] = [mempty]
-- resolve rs (goal : rest) = do
--   Rule h bs <- rs
--   s <- maybeToList (unify goal h)
--   let newGoals = map (apply s) (bs <> rest)
--   s' <- resolve rs newGoals
--   return $ s <> s'
