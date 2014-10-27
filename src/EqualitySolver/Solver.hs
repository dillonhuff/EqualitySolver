module EqualitySolver.Solver(
  eqF, eq, neq,
  var,
  satisfiableInEq) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.UnionFind as U
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Set as S

-- A conjunction of literals
data EqFormula = EqFormula (Set EqLiteral)
                 deriving (Eq, Ord, Show)

eqF = EqFormula . S.fromList

allTerms :: EqFormula -> [EqTerm]
allTerms (EqFormula lits) = L.concat $ L.map extractTerms $ S.toList lits
  where
    extractTerms = \(EqLiteral _ l r) -> [l, r]

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
decideEq f@(EqFormula lits) = do
  addTerms $ allTerms f
  processEqualities $ S.toList lits

data EqState
  = EqState {
    diseqs :: Set (EqTerm, EqTerm),
    pointMap :: Map EqTerm (Point EqTerm)
    }

addNeq :: EqTerm -> EqTerm -> DecideEq Bool
addNeq l r = do
  eqAlready <- sameClass l r
  case eqAlready of
    True -> return False
    False -> return True

getRep :: EqTerm -> DecideEq (Point EqTerm)
getRep t = do
  dt <- getPoint t
  repr dt

sameClass :: EqTerm -> EqTerm -> DecideEq Bool
sameClass l r = do
  repL <- getRep l
  repR <- getRep r
  equivalent repL repR

mergeClasses :: EqTerm -> EqTerm -> DecideEq Bool
mergeClasses l r = do
  repL <- getPoint l
  repR <- getPoint r
  U.union repL repR
  deqs <- lift $ gets diseqs
  classConflict $ S.toList deqs

classConflict :: [(EqTerm, EqTerm)] -> DecideEq Bool
classConflict [] = return True
{-classConflict (nextDis:rest) = do
  s <- sameClass (fst nextDis) (snd nextDis)
  case s of
    True -> return False
    _ -> do
      classConflict rest-}

addEq :: EqTerm -> EqTerm -> DecideEq Bool
addEq l r = do
  eqAlready <- sameClass l r
  case eqAlready of
    True -> return True
    False -> mergeClasses l r

instance Show EqState where
  show = showEqState

showEqState :: EqState -> String
showEqState (EqState dqs pMap) = L.concat $ L.map (show . fst) $ M.toList pMap

newEqState = EqState S.empty M.empty

pointForTerm :: EqTerm -> DecideEq (Maybe (Point EqTerm))
pointForTerm t = do
  pMap <- lift $ gets pointMap
  return $ M.lookup t pMap

getPoint :: EqTerm -> DecideEq (Point EqTerm)
getPoint t = do
  p <- pointForTerm t
  return $ fromJust p

addTerm :: EqTerm -> DecideEq ()
addTerm t = do
  point <- pointForTerm t
  case point of
    Just p -> return ()
    Nothing -> do
      pts <- lift $ gets pointMap
      pt <- fresh t
      lift $ modify $ \eqSt -> eqSt { pointMap = M.insert t pt pts }

addTerms :: [EqTerm] -> DecideEq ()
addTerms [] = return ()
addTerms (t:ts) = do
  addTerm t
  addTerms ts
  return ()

processEqualities :: [EqLiteral] -> DecideEq Bool
processEqualities [] = return True
processEqualities ((EqLiteral Eq l r):ts) = do
  sat <- addEq l r
  case sat of
    True -> do
      processEqualities ts
    False -> return False
processEqualities ((EqLiteral Neq l r):ts) = do
  sat <- addNeq l r
  case sat of
    True -> do
      processEqualities ts
    False -> return False
