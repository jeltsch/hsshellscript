# HsShellScript - Haskell for Unix shell scripting tasks

This is HsShellScript, a library which enables you to use Haskell for tasks which
are typically done by shell scripts. It requires the Glasgow Haskell Compiler.

More elaborate information can be found on the
[HsShellScript Homepage](https://volker-wysk.de/hsshellscript/).

## Status

I've ported HsShellScript to base-4.17.2 (GHC 9.4/9.5). That's the recommended version (as of 2024-04-02). From
what I've learnt, I set the bounds of the version of the base package to ">= 4.17.0.0 && < 5".

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
