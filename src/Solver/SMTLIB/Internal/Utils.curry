--- ----------------------------------------------------------------------------
--- This module provides utilities for interacting with SMT solvers.
---
--- @author  Jan Tikovsky
--- @version May 2021
--- ----------------------------------------------------------------------------
module Solver.SMTLIB.Internal.Utils where

import System.IO       (Handle, hGetLine)

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
