# HsShellScript - Haskell for Unix shell scripting tasks

This is HsShellScript, a library which enables you to use Haskell for tasks which
are typically done by shell scripts. It requires the Glasgow Haskell Compiler.

More elaborate information can be found on the
[HsShellScript Homepage](https://volker-wysk.de/hsshellscript/).

## Status

**NOTE: This package runs only on GHC 8.8**, since there are breaking API changes in later versions. HsShellScript
needs to be ported to the latest GHC version. For that, I need a working Haskell installation with the latest
versions first. My version is from the haskell-platform package in Ubuntu 22.04 LTS. See
[Base package](https://wiki.haskell.org/Base_package) for a list of GHC versions and the included base package
version.

When you have a newer GHC version, you could try to port it by yourself. It's just three simple errors.

## Installation and Usage

Cabal is being used. You can just import the parts of HsShellScript and build your program with cabal.
HsShellScript will be downloaded and installed automatically.

## Documentation

The documentation is in the API documentation. There you'll also find some examples.

## License

HsShellScript is released under the terms of the GNU Lesser General Public License
(LGPL), version 2.1, or any later version.

## Author

Volker Wysk <post@volker-wysk.de>
