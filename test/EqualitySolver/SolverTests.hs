module EqualitySolver.SolverTests() where

import EqualitySolver.Solver
import EqualitySolver.TestUtils

allSolverTests = do
  testFunction satisfiableInEq satCases

satCases =
  [(eqF $ [eq (var "a") (var "b")], True),
   (eqF $ [eq (var "a") (var "b"), neq (var "a") (var "b")], False)]
