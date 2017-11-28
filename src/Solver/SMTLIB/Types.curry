--- ----------------------------------------------------------------------------
--- This module provides types for representing SMT solvers and options
--- in Curry.
---
--- @author  Jan Tikovsky
--- @version November 2017
--- ----------------------------------------------------------------------------
module Solver.SMTLIB.Types where

import Language.SMTLIB.Types (Command)

import Text.Pretty

--- SMT solver configuration
data SMTSolver = SMTSolver { executable :: String, flags :: [String] }

--- Options for SMT solving
--- incremental - solve SMT problems incrementally
--- quiet       - work quietly
--- tracing     - produce SMT-LIB script with all commands used during a session
--- globalCmds  - set commands which are globally valid during a session
data SMTOpts = SMTOpts
  { incremental :: Bool
  , quiet       :: Bool
  , tracing     :: Bool
  , globalCmds  :: [Command]
  }

--- SMT solver error
data SMTError = SolverError String
              | ParserError String
              | OtherError  String
  deriving Show

instance Pretty SMTError where
  pretty (SolverError err) = text "Solver Error:" <+> text err
  pretty (ParserError err) = text "Parser Error:" <+> text err
  pretty (OtherError  err) = text "Error:"        <+> text err
