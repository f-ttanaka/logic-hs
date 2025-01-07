module Stream where

import Common
import Control.Monad (ap, liftM)

-- potentially infinite list
type Stream a = [a]

newtype Diag a = MkDiag {unDiag :: [a]}
  deriving (Show)

instance Functor Diag where
  fmap = liftM

instance Applicative Diag where
  pure x = MkDiag [x]
  (<*>) = ap

instance Monad Diag where
  MkDiag dxs >>= f = MkDiag $ concat (diag (map (unDiag . f) dxs))
    where
      diag :: Stream (Stream a) -> Stream [a]
      diag [] = []
      diag (xs : xss) = lzw (++) [[x] | x <- xs] ([] : diag xss)
