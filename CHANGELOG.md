# Revision history for HsShellScript

## 3.6.4 -- 2024-04-01

* Set the bounds of the version of the base package back to base >= 4.13.0 && < 4.14 for now, since it doesn't
  compile with the newer GHC version on Hackage. This means that newer compilers aren't supported.

## 3.6.3 -- 2024-03-31

* Fixed Hackage version conflict of the base package. See the thread "Hackage: "Build: PlanningFailed"" in the
  mailing list haskell-cafe.

## 3.6.2 -- 2024-03-30

* Haddock out-of-scope warnings.

## 3.6.1 -- 2024-03-30

* Fixed a cabal dependency problem.

## 3.6.0 -- 2024-03-28

* First version using the latest Cabal infrastructure.
* Added pipe_from_full and pipe_from_full2
* Reformatted the source code and the comments such that they aren's so wide.

