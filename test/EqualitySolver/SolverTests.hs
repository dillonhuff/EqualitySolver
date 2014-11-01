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
   (eqF $ [eq (var "a") (var "b"), eq (var "c") (var "b"), neq (var "a") (var "c")], False),
   (eqF $ [eq (var "a") (var "b"), neq (var "c") (var "d"), eq (var "c") (var "b")], True),
   (eqF $ [eq (var "a") (var "b"),
           eq (var "c") (var "d"),
           eq (var "e") (var "f"),
           neq (var "e") (var "b"),
           eq (var "c") (var "a"),
           neq (var "d") (var "b")], False),
   (eqF $ [eq (fun "f" 2 [var "a", var "b"]) (fun "g" 1 [])], True),
   (eqF $ [neq (fun "f" 1 [var "a"]) (fun "f" 1 [var "b"]),
           eq (var "a") (var "b")], False),
   (eqF $ [neq (fun "f" 1 [fun "g" 1 [var "a"]]) (fun "f" 1 [fun "g" 1 [var "b"]]),
           eq (var "a") (var "b")], False),
   (eqF $ [neq (fun "f" 1 [fun "g" 1 [var "a"]]) (fun "f" 1 [fun "g" 1 [var "b"]]),
           neq (var "a") (var "b")], True),
   (eqF $ [neq (fun "f" 2 [var "a", var "b"]) (fun "f" 2 [var "c", var "d"]),
           eq (var "a") (var "e"), eq (var "e") (var "c"), eq (var "b") (var "d")], False)]

