--- Examples for using an SMT solver via Curry

import Either      (partitionEithers, rights)
import Text.Pretty

import Language.SMTLIB

import Solver.SMTLIB.Z3

--- Declare two integer variables
declXY :: [Command]
declXY = declVars [("x", intSort), ("y", intSort)]

--- Declare two integer variables
declAB :: [Command]
declAB = declVars [("a", intSort), ("b", intSort)]

--- Representation of the arithmetic equations
--- x +  y = 10
--- x + 2y = 20
--- as SMT-LIB script
problem1 :: [Command]
problem1 = [assert [ var "x" +% var "y" =% tint 10
                   , var "x" +% (tint 2 *% var "y") =% tint 20
                   ]
           ]

--- Representation of the arithmetic equations
--- 3x +  y = 10
--- 2x + 2y = 28
--- as SMT-LIB script
problem2 :: [Command]
problem2 = [assert [ (tint 3 *% var "x") +% var "y" =% tint 10
                   , (tint 2 *% var "x") +% (tint 2 *% var "y") =% tint 28
                   ]
           ]

--- Representation of the formula
--- b > 4 && a > b && a < 10
--- as SMT-LIB script
problem3 :: [Command]
problem3 = [assert [ var "b" >% tint 4
                   , var "a" >% var "b"
                   , var "a" <% tint 10
                   ]
           ]

--- Solve a single SMT problem with tracing information
--- The trace is dumped to a file in the hidden folder .smt
main1 :: IO ()
main1 = do
  answer <- evalSessions z3 defSMTOpts { globalCmds = declXY, tracing = True } $
              solveSMT problem1
  case answer of
    Left  errs -> putDocLn $ hsep $ map pretty errs
    Right mrsp -> putDocLn $ pretty mrsp


--- Solve two SMT problems in a non-incremental session with status information
main2 :: IO ()
main2 = do
  answers <- evalSessions z3 defSMTOpts { globalCmds = declXY, quiet = False } $ do
    a1 <- solveSMTVars [var "y"] problem1
    a2 <- solveSMTVars [var "x"] problem2
    return $ rights [a1, a2]
  case answers of
    [] -> putStrLn "No solutions found"
    _  -> putDocLn $ hsep $ map pretty answers

--- Example for an incremental SMT session
main3 :: IO ()
main3 = do
  (ss,fs) <- evalSessions z3 defSMTOpts { globalCmds = declAB, incremental = True } $ do
    a1 <- solveSMTVars [var "a"] problem3
    a2 <- solveSMTVars [var "a"] [assert [var "b"  =% tint 8]]
    a3 <- solveSMTVars [var "a"] [assert [var "a" /=% tint 9]]
    return $ partitionEithers [a1, a2, a3]
  putDocLn $ hsep $ map pretty ss
  unless (null fs) $ putDocLn $ hsep $ map pretty fs

--- Example for changing options while solving multiple SMT problems
main4 :: IO ()
main4 = do
  let opts = defSMTOpts { globalCmds = declXY }
  answers <- evalSessions z3 opts $ do
    a1 <- solveSMTVars [var "y"] problem1
    let opts' = opts { quiet = False, tracing = True }
    setSMTOpts opts'
    a2 <- solveSMTVars [var "x"] problem2
    setSMTOpts opts' { quiet = True, globalCmds = declAB }
    a3 <- solveSMTVars [var "a"] problem3
    return $ rights [a1, a2, a3]
  case answers of
    [] -> putStrLn "No solutions found"
    _  -> putDocLn $ hsep $ map pretty answers

--- Transform a 'Doc' to a string and print it to command line
putDocLn :: Doc -> IO ()
putDocLn = putStrLn . pPrint
