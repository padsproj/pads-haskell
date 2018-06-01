# pads-haskell
Massive amounts of useful data are stored and processed in ad hoc formats for
which common tools like parsers, printers, query engines and format converters
are not readily available. Pads/Haskell is a domain-specific language that
facilitates the generation of data processing tools for ad hoc formats.
Pads/Haskell includes features such as dependent, polymorphic and recursive
datatypes, which allow programmers to describe the syntax and semantics of ad
hoc data in a concise, easy-to-read notation. 

The pads haskell repository contains the code for the Haskell binding for PADS.
For more information about the project, see the pads website
(www.padsproj.org).

# Building

`pads-haskell` currently requires GHC 8.2.2 and stack resolver lts-11.3.

## Setup

To generate an appropriate [Stack][1] configuration file and install an
appropriate GHC tool chain:

```bash
$ stack solver    # Updates stack.yaml if necessary
$ stack setup     # Installs ghc in a sandbox for you
```

## Build

To build `pads-haskell`:

```bash
$ stack build
```

# Testing

To run the automated testing infrastructure:

```bash
$ stack test :examples --ghc-options="-ddump-splices"
# Followed by this if you want to see the dumped splice files:
$ find . -name *.dump-splices
```

To run individual tests do:

```bash
$ stack repl
λ> :l Examples.First
...
λ> test
Cases: 89  Tried: 89  Errors: 0  Failures: 0
Counts {cases = 89, tried = 89, errors = 0, failures = 0}
(0.11 secs, 0 bytes)
```

# Contributing and Development

In order to build and view the haddock documentation do the following:

```bash
stack haddock
firefox `find .stack-work -name index.html | grep "html/pads-haskell"`
```

Pull requests are strongly encouraged, though we're more likely to merge them in
a timely fashion if they either add small features to existing modules or are
new PADS descriptions to add to the `examples` directory.

[1]: https://www.stackage.org/
