module EqualitySolver.SolverTests() where

import EqualitySolver.Solver
import EqualitySolver.TestUtils

allSolverTests = do
  testFunction satisfiableInEq satCases

satCases =
  [(eqF $ [eq (var "a") (var "b")], True),
   (eqF $ [neq (var "a") (var "a")], False),
   (eqF $ [neq (var "a") (var "b")], True),
   (eqF $ [neq (var "a") (var "b"), eq (var "a") (var "b")], False),
   (eqF $ [eq (var "a") (var "b"), neq (var "a") (var "b")], False),
   (eqF $ [eq (var "a") (var "b"), eq (var "c") (var "b"), neq (var "a") (var "c")], False)]
