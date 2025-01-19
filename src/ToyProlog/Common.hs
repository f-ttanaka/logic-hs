module ToyProlog.Common
  ( module Relude,
    module Control.Exception.Safe,
    lzw,
  )
where

import Control.Exception.Safe
import Relude

lzw :: (a -> a -> a) -> [a] -> [a] -> [a]
lzw _ [] ys = ys
lzw _ xs [] = xs
lzw f (x : xs) (y : ys) = f x y : lzw f xs ys
