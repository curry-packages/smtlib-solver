--- ----------------------------------------------------------------------------
--- This module provides utilities for interacting with SMT solvers.
---
--- @author  Jan Tikovsky
--- @version November 2017
--- ----------------------------------------------------------------------------
module Solver.SMTLIB.Internal.Utils where

import IO       (Handle, hGetLine)
import FilePath ((</>), (<.>))
import Time     (getLocalTime, calendarTimeToString)

--- Generate a unique file name with the given prefix and file extension
getUniqueFN :: String -> String -> IO String
getUniqueFN prefix ext = do
  tstmp <- getLocalTime
  return $ prefix ++ calendarTimeToString tstmp <.> ext

--- Delimiter (for SMT-LIB command responses)
delim :: String
delim = "END-OF-ANSWER"

--- Reads the contents from an input handle up to the given delimiter
--- and leaves the handle open
hGetUntil :: Handle -> String -> IO String
hGetUntil h d = do
  l@(c:cs) <- hGetLine h
  let l' = if c == '@' then "" else l
  if cs == d then return ""
             else hGetUntil h d >>= \ls -> return (l' ++ '\n' : ls)

--- Perform given monadic action only if boolean condition is satisfied
whenM :: Monad m => Bool -> m () -> m ()
whenM p a = if p then a else return ()

--- Perform given monadic action only if boolean condition is not satisfied
unlessM :: Monad m => Bool -> m () -> m ()
unlessM p = whenM (not p)
