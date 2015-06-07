
PGSolver
========

This is a fork of PGSolver extended with two model-checking tools for the modal mu-calculus and LTL, and two new parity game solvers. The original PGSolver is found at https://github.com/tcsprojects/pgsolver


## PGSolver Copyright Notice

Version 3.3, Copyright (c) 2008-2013

It is developed and maintained by:
- (c) Oliver Friedmann, University of Munich (http://oliverfriedmann.de)
- (c) Martin Lange, University of Kassel (http://carrick.fmv.informatik.uni-kassel.de/~mlange/)


## Installation

- Install OCaml.
- Run ```git submodule update --init``` to checkout all required sub modules
- Create a copy of Config.default, name it Config and modify it to fit your configuration
- Run ```make all```


## Config

There is one config file, ```Config.default``` that has to be edited. It is highly recommended to create a copy of the file with the name Config in the respective directory that is to be edited instead of the original version. The Makefile checks whether a customized configuration file named Config exists and if so it is used instead of the default versions.

The configuration file starts with declarations about where to find all the programs necessary to build the executable PGSolver. Change these lines to point to the full path in which the OCaml compiler, lexer and parser generator are installed unless they are in the current PATH.

You need to give the full path of you OCaml installation directory: ```OCAML_DIR=/usr/lib/ocaml```


## Usage

The model-checking tools are invoked ```bin/mucalcmc <lts> <formula>``` or ```bin/ltlmc <lts> <formula>``` where ```<lts>``` is a labelled transition system and ```<formula>``` is a formula in modal mu-calculus or LTL. For example:
```
$ bin/ltlmc test/mutex/mutex.lts test/mutex/liveness.ltl
```
See the thesis for a specification of input and output.

An solver can be used using ```pgsolver```, e.g.
```
$ bin/randomgame 100 100 2 4 | bin/pgsolver -global vester2 -ve
```
for solving a random game with 100 nodes using Normal-Form Algorithm 2. Please consult ```doc/pgsolver.pdf``` for a more thorough guide to the usage of PGSolver.


## Source

The source of model-checking tool is found in ```src/modelchecker/```.

The solvers are ```vester``` and ```vester2``` found in ```src/solvers/```. Some auxiliary functions for these algorithms are found in ```src/paritygame/```.



