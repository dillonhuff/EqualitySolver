module EqualitySolver.Solver(
  eqF, eq, neq,
  var, fun,
  satisfiableInEq) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Union as U
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

extractTerms (EqLiteral _ l r) = [l, r]

contains :: EqTerm -> EqTerm -> Bool
contains t (Function _ _ args) = tIsArg || tInArg
  where
    tIsArg = L.or $ L.map (==t) args
    tInArg = L.or $ L.map (contains t) args
contains _ _ = False

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
  = Function Name Arity [EqTerm]
  | Variable Name
    deriving (Eq, Ord)

instance Show EqTerm where
  show = showEqTerm

showEqTerm :: EqTerm -> String
showEqTerm (Function name arity args) = name ++ "(" ++ (L.concat $ L.intersperse "," $ L.map show args) ++ ")"
showEqTerm (Variable name) = name

var = Variable
fun = Function

satisfiableInEq :: EqFormula -> Bool
satisfiableInEq formula = fst $ runDecideEq $ decideEq formula

type DecideEq a = StateT EqState (UnionM EqTerm) a

runDecideEq :: DecideEq a -> (a, EqState)
runDecideEq decide =  run $ runStateT decide newEqState

decideEq :: EqFormula -> DecideEq Bool
decideEq f@(EqFormula lits) = do
  addTerms $ allTerms f
  buildContainsMap (allTerms f) (allTerms f)
  processEqualities $ S.toList lits

data EqState
  = EqState {
    diseqs :: Set (EqTerm, EqTerm),
    pointMap :: Map EqTerm Node,
    superTerms :: Map Node [EqTerm]
    }

termsContaining :: EqTerm -> DecideEq [EqTerm]
termsContaining t = do
  pt <- getRep t
  sts <- gets superTerms
  case M.lookup pt sts of
    Just ts -> return ts
    Nothing -> error $ "Term " ++ show t ++ " not in superTerms"

addNeq :: EqTerm -> EqTerm -> DecideEq Bool
addNeq l r = do
  eqAlready <- sameClass l r
  deqs <- gets diseqs
  modify $ \s -> s { diseqs = S.insert (l, r) deqs }
  case eqAlready of
    True -> return False
    False -> return True

getRep :: EqTerm -> DecideEq Node
getRep t = do
  dt <- getNode t
  (repr, lab) <- U.lookup dt
  return repr

sameClass :: EqTerm -> EqTerm -> DecideEq Bool
sameClass l r = do
  repL <- getRep l
  repR <- getRep r
  return $ repL == repR

defaultMerge :: EqTerm -> EqTerm -> (EqTerm, [a])
defaultMerge l r = (l, [])
mergeClasses :: EqTerm -> EqTerm -> DecideEq (Bool, [EqLiteral])
mergeClasses l r = do
  repL <- getNode l
  repR <- getNode r
  termsWithL <- termsContaining l
  termsWithR <- termsContaining r
  res <- U.merge defaultMerge repL repR
  case res of
    Nothing -> return (True, [])
    _ -> do
      deqs <- gets diseqs
      conflict <- classConflict $ S.toList deqs
      case conflict of
        True -> return (False, [])
        False -> do
          newCong <- findCongruences termsWithL termsWithR
          return (True, newCong)

findCongruences :: [EqTerm] -> [EqTerm] -> DecideEq [EqLiteral]
findCongruences [] rs = return []
findCongruences (l:ls) rs = do
  congWithL <- congruentWith l rs
  rest <- findCongruences ls rs
  return $ congWithL ++ rest

congruentWith :: EqTerm -> [EqTerm] -> DecideEq [EqLiteral]
congruentWith l [] = return []
congruentWith l (r:rs) = do
  areCong <- congruent l r
  rest <- congruentWith l rs
  case areCong of
    True -> return $ (EqLiteral Eq l r):rest
    False -> return rest

congruent :: EqTerm -> EqTerm -> DecideEq Bool
congruent (Function n1 a1 args1) (Function n2 a2 args2) = do
  case n1 /= n2 || a1 /= a2 of
    True -> return False
    False -> equivalentArgs args1 args2

equivalentArgs :: [EqTerm] -> [EqTerm] -> DecideEq Bool
equivalentArgs [] [] = return $ True
equivalentArgs (l:ls) (r:rs) = do
  ln <- getNode l
  rn <- getNode r
  case ln == rn of
    True -> equivalentArgs ls rs
    False -> return False

classConflict :: [(EqTerm, EqTerm)] -> DecideEq Bool
classConflict [] = return False
classConflict (nextDis:rest) = do
  s <- sameClass (fst nextDis) (snd nextDis)
  case s of
    True -> return False
    _ -> do
      classConflict rest

addEq :: EqTerm -> EqTerm -> DecideEq (Bool, [EqLiteral])
addEq l r = do
  eqAlready <- sameClass l r
  case eqAlready of
    True -> return (True, [])
    False -> mergeClasses l r

instance Show EqState where
  show = showEqState

showEqState :: EqState -> String
showEqState (EqState dqs pMap _) = L.concat $ L.map (show . fst) $ M.toList pMap

newEqState = EqState S.empty M.empty M.empty

nodeForTerm :: EqTerm -> DecideEq (Maybe Node)
nodeForTerm t = do
  pMap <- gets pointMap
  return $ M.lookup t pMap

getNode :: EqTerm -> DecideEq Node
getNode t = do
  p <- nodeForTerm t
  return $ fromJust p

addTerm :: EqTerm -> DecideEq ()
addTerm t = do
  point <- nodeForTerm t
  case point of
    Just p -> return ()
    Nothing -> do
      pts <- gets pointMap
      pt <- new t
      modify $ \eqSt -> eqSt { pointMap = M.insert t pt pts }

addTerms :: [EqTerm] -> DecideEq ()
addTerms [] = return ()
addTerms (t:ts) = do
  addTerm t
  addTerms ts
  return ()

buildContainsMap :: [EqTerm] -> [EqTerm] -> DecideEq ()
buildContainsMap [] _ = return ()
buildContainsMap (l:ls) r = do
  tc <- allTermsContaining l r
  oldSup <- gets superTerms
  n <- getNode l
  modify $ \eqSt -> eqSt { superTerms = M.insert n tc oldSup }
  buildContainsMap ls r

allTermsContaining :: EqTerm -> [EqTerm] -> DecideEq [EqTerm]
allTermsContaining l [] = return []
allTermsContaining l (r:rs) = do
  case contains l r of
    True -> do
      tc <- allTermsContaining l rs
      return $ r:tc
    False -> allTermsContaining l rs

processEqualities :: [EqLiteral] -> DecideEq Bool
processEqualities [] = return True
processEqualities ((EqLiteral Eq l r):ts) = do
  (sat, newEqs) <- addEq l r
  case sat of
    True -> do
      processEqualities (newEqs ++ ts)
    False -> return False
processEqualities ((EqLiteral Neq l r):ts) = do
  sat <- addNeq l r
  case sat of
    True -> do
      processEqualities ts
    False -> return False
