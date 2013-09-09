# Functionally Reactive Solar System

## About

This is a simple program written to demonstrate the following principles:

- Functional Reactive Programming using reactive-banana
- Using GTK in Haskell
- Using OpenGL in Haskell
- Using GTKGLExt in Haskell

## Installation

### Prerequisites on Linux

Development of this program is largely happening on Debian GNU/Linux's
"unstable" branch.  reactive-solar.cabal contains build-depends for
the Haskell side of things, but base libraries they depend on will
need to be installed as well.  Under Debian, the package names of
these are:

* libgtk2.0-dev
* libgtkglext1-dev
* libftgl2-dev

Similar packages can be found on other Linux platforms.

As for GHC, I recommend installing the base system through Debian's
package management then using *cabal* to install all other dependencies.

### Prerequisites for Mac OS X

GHC, GTK+ and GTKGLExt can be installed through
[homebrew](http://mxcl.github.com/homebrew/) and then Haskell
dependencies can be handled with *cabal*.  The homebrew package names
are:

* haskell-platform
* gtk+
* gtkglext

### Prerequisites for Haskell

This program uses some Haskell libraries that are not in Hackage:

* [reactive-banana-gtk](https://github.com/conklech/reactive-banana-gtk)
* [a fork of FTGL](https://github.com/Peaker/FTGL)
    * Requires FTGL library (see Linux Preqs)

Download them (using git) and install by running *cabal-install* from
its top level directory.

*cabal* should handle the rest of the Haskell prerequisites. Run *cabal
 configure && cabal build* to build the program in place.  

### Running

Currently, this program uses data files, and cabal's data-files
support has not been integrated in yet, so *cabal install* will not do
the right thing and should not be used.  Load *reactive-solar.hs* into
ghci and type *main* to run the program for the time being.

## Meta

Author: James Curbo <james@curbo.org>

Written for the author's Capstone Research Project, part of the degree
plan for earning a Masters of Science in Computer Science at [Capitol
College](http://www.capitol-college.edu) in Laurel, MD.

This project is available at both [bitbucket](http://bitbucket.org/jcurbo/capitol_cs714)
and [github](http://github.com/jcurbo/functionally-reactive-solar-system).
