module EqualitySolver.Solver(
  eqF, eq, neq,
  var,
  satisfiableInEq) where

import Data.List as L
import Data.Set as S

-- A conjunction of literals
data EqFormula = EqFormula (Set EqLiteral)
                 deriving (Eq, Ord, Show)

eqF = EqFormula . S.fromList

data EqLiteral
  = EqLiteral Predicate EqTerm EqTerm
    deriving (Eq, Ord, Show)

eq = EqLiteral Eq
neq = EqLiteral Neq

data Predicate
  = Eq
  | Neq
    deriving (Eq, Ord, Show)

type Arity = Int
type Name = String

data EqTerm
  = Function Arity
  | Variable Name
    deriving (Eq, Ord, Show)

var = Variable

satisfiableInEq :: EqFormula -> Bool
satisfiableInEq formula = True
