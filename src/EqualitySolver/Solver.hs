module EqualitySolver.Solver(
  eqF, eq, neq,
  var,
  satisfiableInEq) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.UnionFind as U
import Data.List as L
import Data.Map as M
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
  = Function Arity [EqTerm]
  | Variable Name
    deriving (Eq, Ord, Show)

var = Variable

satisfiableInEq :: EqFormula -> Bool
satisfiableInEq formula = fst $ runDecideEq $ decideEq formula

type DecideEq a = UnionFindT EqTerm (StateT EqState Identity) a

runDecideEq :: DecideEq a -> (a, EqState)
runDecideEq decide = runIdentity $ runStateT (runUnionFind decide) newEqState

decideEq :: EqFormula -> DecideEq Bool
decideEq f = do
  return False

data EqState
  = EqState {
    diseqs :: Set (EqTerm, EqTerm),
    pointMap :: Map EqTerm (Point EqTerm)
    }

newEqState = EqState S.empty M.empty

