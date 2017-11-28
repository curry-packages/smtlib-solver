--- ----------------------------------------------------------------------------
--- This module provides operations for an interactive communication with
--- SMT solvers - which implement the SMT-LIB interface - via stdin and stdout.
--- Currently only the Z3 SMT solver is supported.
---
--- @author  Jan Tikovsky, Marcellus Siegburg
--- @version November 2017
--- ----------------------------------------------------------------------------
module Solver.SMTLIB.Session
  ( module Solver.SMTLIB.Types
  , SMTError (..), SMTOpts (..), SMTSess, SMTSolver (..)
  , defSMTOpts, evalSessions, setSMTOpts, solveSMT, solveSMTVars
  , liftIOA
  ) where

import Language.SMTLIB.Types (Command, ModelRsp, Term, ValuationPair)

import Solver.SMTLIB.Internal.Interaction
import Solver.SMTLIB.Internal.Utils       (whenM)
import Solver.SMTLIB.Types

--- default options for SMT solving
defSMTOpts :: SMTOpts
defSMTOpts = SMTOpts
  { incremental = False
  , quiet       = True
  , tracing     = False
  , globalCmds  = []
  }

--- Evaluate SMT sessions by applying given solver and options
evalSessions :: SMTSolver -> SMTOpts -> SMTSess a -> IO a
evalSessions = evalSessionsImpl

--- Set options for SMT solving
setSMTOpts :: SMTOpts -> SMTSess ()
setSMTOpts opts = evalSession $ do
  resetSession
  modify $ \s -> s { options = opts }

--- Solve the SMT problem specified by the given SMT-LIB commands and
--- try to find a binding for all variables used
solveSMT :: [Command] -> SMTSess (Either [SMTError] [ModelRsp])
solveSMT cmds = evalSession $ do
  bufferGlobalDefs
  info "Asserting definitions and constraints"
  sendCmds cmds
  info "Checking satisfiability of constraints"
  isSat <- checkSat
  case isSat of
    Sat -> do
      info "Satisfiable -> Getting model for SMT problem"
      m <- getModel
      optReset
      return m
    res -> do
      info "No model found for given SMT problem"
      optReset
      return $ Left $ res2Msgs res

--- Solve the SMT problem specified by the given SMT-LIB commands and
--- try to find a binding for all given variables
solveSMTVars :: [Term] -> [Command]
             -> SMTSess (Either [SMTError] [ValuationPair])
solveSMTVars vars cmds = evalSession $ do
  bufferGlobalDefs
  info "Asserting definitions and constraints"
  sendCmds cmds
  info "Checking satisfiability of constraints"
  isSat <- checkSat
  case isSat of
    Sat -> do
      info "Satisfiable -> Getting bindings of given variables for SMT problem"
      vps <- getValues vars
      optReset
      return vps
    res -> do
      info "No variable bindings found for given SMT problem"
      optReset
      return $ Left $ res2Msgs res
