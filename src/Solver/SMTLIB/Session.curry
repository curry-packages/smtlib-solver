--- ----------------------------------------------------------------------------
--- This module provides operations for an interactive communication with
--- SMT solvers - which implement the SMT-LIB interface - via stdin and stdout.
--- Currently only the Z3 SMT solver is supported.
---
--- @author  Jan Tikovsky, Marcellus Siegburg
--- @version August 2021
--- ----------------------------------------------------------------------------
module Solver.SMTLIB.Session
  ( module Solver.SMTLIB.Types
  , SMTError (..), SMTOpts (..), SMTSess, SMTSolver (..)
  , defSMTOpts, evalSessions, freshSMTVars, setSMTOpts, solveSMT, solveSMTVars
  , solveAllSMTVars, liftIOA
  ) where

import Control.Monad.Extra     (concatMapM)
import Data.List               (nub)
import Data.Maybe              (fromJust)

import Language.SMTLIB.Goodies (assert, (/=%))
import Language.SMTLIB.Types   ( Command (Push, Pop), ModelRsp, Sort, Term
                               , ValuationPair
                               )

import Solver.SMTLIB.Internal.Interaction
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

--- Get n fresh variables of given sort
freshSMTVars :: Int -> Sort -> SMTSess [Term]
freshSMTVars n s = evalSession $ declareVars n s

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

--- Solve the SMT problem specified by the given SMT-LIB commands and
--- try to find all bindings for the given variable.
--- The given integer determines how many counter examples are returned at
--- maximum for each variable.
solveAllSMTVars :: [Term] -> [Command] -> Int
                -> SMTSess (Either [SMTError] [[ValuationPair]])
solveAllSMTVars vars cmds i = evalSession $ do
  bufferGlobalDefs
  info "Asserting definitions and constraints"
  sendCmds cmds
  info "Checking satisfiability of constraints"
  isSat <- checkSat
  case isSat of
    Sat -> do
      info "Satisfiable -> Getting bindings of given variables for SMT problem"
      vps <- getValues vars
      case vps of
        Right vps' -> do
          vpss <- concatMapM (getCounterExamples vps' i) vars
          optReset
          return $ Right $ nub vpss
        Left e     -> optReset >> (return $ Left e)
    Unsat -> do
      info "No variable bindings found for given SMT problem"
      optReset
      return $ Right []
    res -> do
      info "An error occurred while solving given SMT problem"
      optReset
      return $ Left $ res2Msgs res
  where
    getCounterExamples :: [ValuationPair] -> Int -> Term
                       -> SMT [[ValuationPair]]
    getCounterExamples vps i' var = do
      bufferCmds [Push 1]
      vpss <- assertCounterExamples (fromJust $ lookup var vps) [vps] var i'
      bufferCmds [Pop 1]
      return vpss

    assertCounterExamples :: Term -> [[ValuationPair]] -> Term -> Int
                          -> SMT [[ValuationPair]]
    assertCounterExamples v vpss var i' = do
      sendCmds [assert [var /=% v]]
      isSat <- checkSat
      case isSat of
        Sat -> do
          vps <- getValues vars
          case vps of
            Right vps' | i' > 1 -> assertCounterExamples
                                     (fromJust $ lookup var vps')
                                     (vps':vpss) var (i' - 1)
            _                   -> return vpss
        _ -> return vpss
