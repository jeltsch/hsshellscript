# Revision history for HsShellScript

## 3.6.4 -- 2024-04-03

* Ported HsShellScript to GHC 9.4.8. That was just a trivial change.
* Set the bounds of the version of the base package to ">= 4.17.0.0 && < 5".
* Removed terminal_width_ioe and terminal_width from Args.hs-boot.

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
