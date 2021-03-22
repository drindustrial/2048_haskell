# 2048 on Haskell

[![Build Status](https://travis-ci.org/iu-haskell-spring-2020/project-template.svg?branch=master)](https://travis-ci.org/iu-haskell-spring-2020/project-template)

This is an implementation of [2048 game](https://github.com/gabrielecirulli/2048) on Haskell. To move a field press W-A-S-D; to get the next best move, calculated by computer, press "B".
### Prerequisites

This project relies on the [Haskell Stack tool](https://docs.haskellstack.org/en/stable/README/).

It is recommended that you follow the instructions on how to install Haskell with Stack on [Haskell's official Downloads page](https://www.haskell.org/downloads/#stack).

## Build

To build this project simply run

```sh
stack build
```

This will install all dependencies, including a proper version of GHC
(which should be there already if you have Haskell Platform 8.6.5).

## Run

This project has one executable that you can run with

```
stack exec my-project-exe
```

During development it is recommended a combination of `build` and `exec`:

```
stack build && stack exec my-project-exe
```

Alternatively, you can run

```
stack build file-watch
```

For continuous builds in the background.

