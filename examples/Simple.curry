--- Examples for using an SMT solver via Curry

import Either      (partitionEithers, rights)
import Text.Pretty

import Language.SMTLIB

import Solver.SMTLIB.Z3

--- Representation of the arithmetic equations
--- x +  y = 10
--- x + 2y = 20
--- as SMT-LIB script
problem1 :: Term -> Term -> [Command]
problem1 x y = [assert [x + y =% 10, x + (2 * y) =% 20]]

--- Representation of the arithmetic equations
--- 3x +  y = 10
--- 2x + 2y = 28
--- as SMT-LIB script
problem2 :: Term -> Term -> [Command]
problem2 x y = [assert [3 * x + y =% 10, 2 * x + 2 * y =% 28]]

--- Representation of the formula
--- b > 4 && a > b && a < 10
--- as SMT-LIB script
problem3 :: Term -> Term -> [Command]
problem3 a b = [assert [b >% 4, a >% b, a <% 10]]

--- Solve a single SMT problem with tracing information
--- The trace is dumped to a file in the hidden folder .smt
main1 :: IO ()
main1 = do
  answer <- evalSessions z3 defSMTOpts { tracing = True } $ do
    [x,y] <- freshSMTVars 2 intSort
    solveSMT (problem1 x y)
  case answer of
    Left  errs -> putDocLn $ hsep $ map pretty errs
    Right mrsp -> putDocLn $ pretty mrsp

--- Solve two SMT problems in a non-incremental session with status information
main2 :: IO ()
main2 = do
  answers <- evalSessions z3 defSMTOpts { quiet = False } $ do
    [x,y] <- freshSMTVars 2 intSort
    a1    <- solveSMTVars [y] (problem1 x y)
    a2    <- solveSMTVars [x] (problem2 x y)
    return $ rights [a1, a2]
  case answers of
    [] -> putStrLn "No solutions found"
    _  -> putDocLn $ hsep $ map pretty answers

--- Example for an incremental SMT session
main3 :: IO ()
main3 = do
  (ss,fs) <- evalSessions z3 defSMTOpts { incremental = True } $ do
    [a,b] <- freshSMTVars 2 intSort
    a1    <- solveSMTVars [a] (problem3 a b)
    a2    <- solveSMTVars [a] [assert [b  =% 8]]
    a3    <- solveSMTVars [a] [assert [a /=% 9]]
    return $ partitionEithers [a1, a2, a3]
  putDocLn $ hsep $ map pretty ss
  unless (null fs) $ putDocLn $ hsep $ map pretty fs

--- Example for changing options while solving multiple SMT problems
main4 :: IO ()
main4 = do
  answers <- evalSessions z3 defSMTOpts $ do
    [x,y] <- freshSMTVars 2 intSort
    a1 <- solveSMTVars [y] (problem1 x y)
    let opts = defSMTOpts { quiet = False, tracing = True }
    setSMTOpts opts
    a2 <- solveSMTVars [x] (problem2 x y)
    setSMTOpts opts { quiet = True }
    a3 <- solveSMTVars [x] (problem3 x y)
    return $ rights [a1, a2, a3]
  case answers of
    [] -> putStrLn "No solutions found"
    _  -> putDocLn $ hsep $ map pretty answers

--- Transform a 'Doc' to a string and print it to command line
putDocLn :: Doc -> IO ()
putDocLn = putStrLn . pPrint
