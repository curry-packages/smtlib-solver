--- ----------------------------------------------------------------------------
--- This module provides a session monad to manage an interactive and
--- incremental SMT solver session.
--- Furthermore it includes abstractions for well-known SMT-LIB commands
--- in Curry which are required during the interaction with an SMT solver.
---
--- @author  Jan Tikovsky, Marcellus Siegburg
--- @version January 2018
--- ----------------------------------------------------------------------------
module Solver.SMTLIB.Internal.Interaction where

import System.IO (Handle, hClose, hFlush, hPutStr)
import IOExts    (execCmd)

import Text.Pretty

import           Language.SMTLIB.Files        (writeSMTDump)
import           Language.SMTLIB.Goodies      (comment, echo, isEcho, var)
import           Language.SMTLIB.Parser       (parseCmdRsps)
import           Language.SMTLIB.Pretty
import qualified Language.SMTLIB.Types as SMT

import Solver.SMTLIB.Internal.Utils
import Solver.SMTLIB.Types

--- An SMT solver session includes
---   * handles for communicating with the solver
---   * a buffer for SMT-LIB commands
---   * a trace of SMT-LIB commands (only required for debugging purposes)
---   * SMT options
---   * an index for fresh variables
---   * a list of global SMT-LIB declarations
data SMTSession = SMTSession
  { handles     :: (Handle, Handle, Handle)
  , buffer      :: [SMT.Command]
  , trace       :: [SMT.Command]
  , options     :: SMTOpts
  , fresh       :: Int
  , globalDecls :: [SMT.Command]
  }

--- Session monad maintaining session information during multiple SMT sessions
data SMTSess a = SMTSess { runSMTSess :: SMTSession -> SMT (a, SMTSession) }

instance Monad SMTSess where
  return x = SMTSess $ \s -> return (x, s)

  m >>= f = SMTSess $ \s -> do
    (r, s') <- runSMTSess m s
    runSMTSess (f r) s'

--- Get SMT session
getSess :: SMTSess SMTSession
getSess = SMTSess $ \s -> return (s, s)

--- Set an SMT session
putSess :: SMTSession -> SMTSess ()
putSess s = SMTSess $ \_ -> return ((), s)

--- Evaluate multiple SMT sessions
evalSess :: SMTSess a -> SMT a
evalSess smtSess = do
  s <- get
  runSMTSess smtSess s >>= \(r, s') -> put s' >> return r

--- Evaluate SMT sessions by applying given solver and options
evalSessionsImpl :: SMTSolver -> SMTOpts -> SMTSess a -> IO a
evalSessionsImpl solver opts as = do
  s       <- startSession solver opts
  (r, s') <- runSMT (evalSess as >>= \res -> closeSession >> return res) s
  termSession s'
  whenM (tracing $ options s') (dumpSession s')
  return r

--- SMT monad maintaining session information while performing SMT actions
--- during a single SMT session
data SMT a = SMT { runSMT :: SMTSession -> IO (a, SMTSession) }

instance Monad SMT where
  return x = SMT $ \s -> return (x, s)

  m >>= f = SMT $ \s -> do
    (r, s') <- runSMT m s
    runSMT (f r) s'

--- Evaluate an SMT action
evalSMT :: SMT a -> SMTSession -> IO a
evalSMT smt s = runSMT smt s >>= return . fst

--- Execute an SMT action
execSMT :: SMT a -> SMTSession -> IO SMTSession
execSMT smt s =
  runSMT smt s >>= return . snd

gets :: (SMTSession -> a) -> SMT a
gets f = SMT $ \s -> return (f s, s)

--- Get SMT session
get :: SMT SMTSession
get = gets id

--- Set SMT session
put :: SMTSession -> SMT ()
put s = SMT $ \_ -> return ((), s)

--- Get handle for stdin
getStdin :: SMT Handle
getStdin = gets ((\(x, _, _) -> x) . handles)

--- Get handle for stdout
getStdout :: SMT Handle
getStdout = gets ((\(_, y, _) -> y) . handles)

--- Get buffered SMT commands and empty buffer
takeBuffer :: SMT [SMT.Command]
takeBuffer = do
  buf <- gets buffer
  modify $ \s -> s { buffer = [] }
  return buf

--- Get trace of SMT commands
getTrace :: SMT [SMT.Command]
getTrace = gets trace

--- Get global commands set in the SMT options
getGlobalCmds :: SMT [SMT.Command]
getGlobalCmds = gets (globalCmds . options)

--- Check if incremental SMT solving is activated
isIncremental :: SMT Bool
isIncremental = gets (incremental . options)

--- Get global declarations
getGlobalDecls :: SMT [SMT.Command]
getGlobalDecls = gets globalDecls

--- Modify an SMT session by applying the given function
modify :: (SMTSession -> SMTSession) -> SMT ()
modify f = SMT $ \s -> return ((), f s)

--- Lift an IO action directly to the SMT session monad
liftIOA :: IO a -> SMTSess a
liftIOA = liftSMT . liftIO2SMT

--- Lift an SMT action to the SMT session monad
liftSMT :: SMT a -> SMTSess a
liftSMT smt = SMTSess $ \s -> smt >>= (\x -> return (x, s))

--- Lift an IO action to the SMT monad
liftIO2SMT :: IO a -> SMT a
liftIO2SMT io = SMT $ \s -> io >>= (\x -> return (x, s))

--- Evaluate a singe SMT session
--- (i.e. lift SMT action to Session monad and thread session)
evalSession :: SMT a -> SMTSess a
evalSession smt = do
  s         <- getSess
  (res, s') <- liftIOA $ runSMT smt s
  putSess s'
  return res

--- ----------------------------------------------------------------------------
--- High-level SMT solver interaction
--- ----------------------------------------------------------------------------

--- SMT solver result
data SMTResult = Error  [SMTError]
            | Unsat
            | Unknown
            | Sat
            | Model  [SMT.ModelRsp]
            | Values [SMT.ValuationPair]
  deriving Show

instance Pretty SMTResult where
  pretty (Error msgs) = vsep $ map pretty msgs
  pretty Unsat        = text "unsat"
  pretty Unknown      = text "unknown"
  pretty Sat          = text "sat"
  pretty (Model   ms) = parent (map pretty ms)
  pretty (Values vps) = parent (map ppValPair vps)

--- Transform a command response to an error message
rsp2Msg :: SMT.CmdResponse -> SMTError
rsp2Msg rsp = case rsp of
  SMT.ErrorRsp            msg -> SolverError  msg
  SMT.CheckSatRsp SMT.Unknown -> SolverError  "Unknown"
  SMT.UnsupportedRsp          -> SolverError  "Unsupported command"
  SMT.CheckSatRsp SMT.Unsat   -> SolverError  "Unsat"
  _                           -> OtherError $ "Unexpected response: " ++ show rsp

--- Transform a result to list of error messages
res2Msgs :: SMTResult -> [SMTError]
res2Msgs res = case res of
  Error msgs -> msgs
  Unsat      -> [SolverError "Unsat"]
  Unknown    -> [SolverError "Unknown"]
  _          -> [OtherError ("Unexpected result: " ++ show res)]

--- Declare n fresh SMT variables of given sort
declareVars :: Int -> SMT.Sort -> SMT [SMT.Term]
declareVars n sort = do
  s <- get
  let v     = fresh s
      names = map (('x' :) . show) [v .. v + n - 1]
  put s { fresh       = v + n
        , globalDecls = globalDecls s ++ map (flip SMT.DeclareConst sort) names
        }
  return $ map var names

--- Check for syntactic errors as well as for satisfiability of the assertions
checkSat :: SMT SMTResult
checkSat = do
  errMsg <- getDelimited
  -- check for syntactic errors, type mismatches etc.
  case parseCmdRsps errMsg of
    Left  msg                -> return $ Error [ParserError msg]
    Right rs | not (null rs) -> return $ Error (map rsp2Msg rs)
             | otherwise     -> do
      sendCmds [SMT.CheckSat]
      satMsg <- getDelimited
      -- check satisfiability
      case parseCmdRsps satMsg of
        Left  msg                           -> return $ Error [ParserError msg]
        Right [SMT.CheckSatRsp SMT.Unknown] -> return $ Unknown
        Right [SMT.CheckSatRsp SMT.Unsat]   -> return $ Unsat
        Right [SMT.CheckSatRsp SMT.Sat]     -> return $ Sat
        Right rsps                          -> return $ Error $ map rsp2Msg rsps

--- Get a model for the current assertions on the solver stack
getModel :: SMT (Either [SMTError] [SMT.ModelRsp])
getModel = do
  sendCmds [SMT.GetModel]
  modelMsg <- getDelimited
  case parseCmdRsps modelMsg of
    Left  msg                 -> return $ Left  $ [ParserError msg]
    Right [SMT.GetModelRsp m] -> return $ Right $ m
    Right rsps                -> return $ Left  $ map rsp2Msg rsps

--- Get bindings for given variables for the current assertions
--- on the solver stack
getValues :: [SMT.Term] -> SMT (Either [SMTError] [SMT.ValuationPair])
getValues ts = do
  sendCmds [SMT.GetValue ts]
  valMsg <- getDelimited
  case parseCmdRsps valMsg of
    Left  msg                 -> return $ Left  $ [ParserError msg]
    Right [SMT.GetValueRsp m] -> return $ Right $ m
    Right rsps                -> return $ Left  $ map rsp2Msg rsps

--- Buffer global definitions in SMT session
bufferGlobalDefs :: SMT ()
bufferGlobalDefs = do
  globals <- getGlobalDecls >>= \ds   ->
             getGlobalCmds  >>= \cmds -> return (ds ++ cmds)
  unlessM (null globals) $ do
    info "Asserting global definitions"
    bufferCmds $ (comment "----- BEGIN GLOBAL DEFINITIONS -----") : globals
      ++ [comment "----- END   GLOBAL DEFINITIONS -----"]
    isInc <- isIncremental
    whenM isInc $ modify $ \s -> s { options = (options s) { globalCmds = [] }
                                   , globalDecls = []
                                   }

--- Reset SMT session (by resetting the SMT solver stack)
resetSession :: SMT ()
resetSession = modify $ \s -> s { buffer = [SMT.Reset] }

--- Optional reset of SMT session (in case of non-incremental solving)
optReset :: SMT ()
optReset = isIncremental >>= \isInc -> unlessM isInc $ do
  info "Resetting SMT solver stack"
  resetSession

--- Optional tracing of SMT-LIB commands in the given buffer
optTracing :: [SMT.Command] -> SMT ()
optTracing buf = do
  s <- get
  whenM (tracing $ options s) (put s { trace = trace s ++ buf })

--- Close SMT session
closeSession :: SMT ()
closeSession = sendCmds [SMT.Exit]

--- ----------------------------------------------------------------------------
--- Low-level SMT solver interaction
--- ----------------------------------------------------------------------------

--- Start SMT solver process and initialize fresh SMT session
startSession :: SMTSolver -> SMTOpts -> IO SMTSession
startSession solver opts = do
  unlessM (quiet opts) $ putStrLn $ "Starting " ++ sname ++ " session."
  hs <- execCmd $ unwords $ sname : flags solver
  return $ SMTSession hs [] [] opts 1 []
 where sname = executable solver

--- Terminate SMT solver process
termSession :: SMTSession -> IO ()
termSession (SMTSession (i, o, e) _ _ opts _ _) = do
  unlessM (quiet opts) $ putStrLn "Terminating session."
  hClose i
  hClose o
  hClose e

--- Produce dump of SMT-LIB commands used during an SMT session
dumpSession :: SMTSession -> IO ()
dumpSession s = writeSMTDump "smtDump" (rmvEchos $ trace s)

--- Buffer given SMT-LIB commands
bufferCmds :: [SMT.Command] -> SMT ()
bufferCmds cmds = modify $ \s -> s { buffer = buffer s ++ cmds }

--- Send SMT-LIB commands to SMT solver
sendCmds :: [SMT.Command] -> SMT ()
sendCmds cmds = do
  bufferCmds (cmds ++ [echo delim])
  sin <- getStdin
  buf <- takeBuffer
  liftIO2SMT $ hPutStr sin (showSMT buf) >> hFlush sin
  optTracing buf

--- Get response string of an SMT solver
getDelimited :: SMT String
getDelimited = getStdout >>= liftIO2SMT . flip hGetUntil delim

--- Write status information to the command line
--- when quiet option is set to False
info :: String -> SMT ()
info msg = get >>= \s -> unlessM (quiet (options s)) $ liftIO2SMT $ putStrLn msg

--- Remove all 'Echo' commands
rmvEchos :: [SMT.Command] -> [SMT.Command]
rmvEchos = filter (not . isEcho)
