module ToyProlog.Data.Term where

import ToyProlog.Common

type Var = String

type Symbol = String

data Term
  = TVar Var
  | TFunc Symbol [Term]
  deriving (Show, Eq)

data Rule = Rule Term [Term]

type Query = Term
