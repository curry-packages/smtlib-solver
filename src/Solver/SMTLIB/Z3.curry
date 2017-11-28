--- ----------------------------------------------------------------------------
--- This module provides the SMT solver configuration for the z3 solver.
---
--- @author  Jan Tikovsky
--- @version November 2017
--- ----------------------------------------------------------------------------

module Solver.SMTLIB.Z3
  ( module Solver.SMTLIB.Session
  , z3
  ) where

import Solver.SMTLIB.Session

--- z3 solver configuration
z3 :: SMTSolver
z3 = SMTSolver { executable = "z3", flags = ["-smt2", "-in"] }
