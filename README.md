smtlib-solver
=============

This package provides general operations for an interactive communication with
SMT solvers via stdin and stdout.
It makes use of the representation of SMT-LIB scripts in Curry.
There is also support for specific SMT solvers.
Currently, only the Z3 SMT solver is supported.

In the future, when there is support for further SMT solvers, there should be
a module `Solver.SMTLIB.AllSolvers` to run all or any of the provided solvers.
Furthermore there should be a module `Solver.SMTLIB` which reexports
`Solver.SMTLIB.AllSolvers` and `Solver.SMTLIB.Session`.

The example folder includes a Curry module demonstrating some exemplary calls
of the SMT solving operations provided by this library.
