# Nixec

Nixec is a evaluation framework partly build on [nix](nixos.org). Nixec
aims to remove the hassle of using nix for repeatable evaluations and
tests of systems. The ultimate goal is that Nixec is going to be:

-   Reproducible: If it works on your computer it should also work on 
    mine.

-   Composeable: When you know how to compute the parts computing 
    the whole should be easy.

-   Idempotent: You only have to re-run the parts that have changed
    when you change your code.

-   Offloadable: You should not have to run your evaluation setup on your
    computer, when you have better servers available.

-   Exportable: It should be possible to export Nixec to multiple
    formats. Most important is:

    -   (TODO) Plain: Bash scripts and folders only, with instructions
        about how to build each step.

    -   (TODO) As a virtual box: A fully contained benchmarks suite
        ready to run from the command line.

    -   (TODO) As a docker container: Pretty much the same idea.


## Overview

Nixec is written in Haskell and it contains of two parts. First, 
an easy to use single entry point.

### `nixec`: the executable.

`nixec` is indented to be a simple tool that 

-   `nixec init <name>`: Create a new directory with the correct
    structure.

-   `nixec run <target>`: Run a certain target. The product will 
    be executed as a nix-script and placed in the `result` folder.

-   `nixec export [<target>]`: Export certain target. 

### `Nixec`: the library

### The `nixec` structure

## Roadmap

-    Compute the execution tree using nix.
-    TODO: Create initial representation that can produce an execution tree.
-    TODO: Create executable, which can set everything up.
-    TODO: Work on export options.
-    TODO: Write documentation.

## Setup Instructions

### Setting up remote builders

To setup remote builders create a file with servers with nixos installed. You 
can add then by adding the option to the `~/.config/nix/nix.conf` file:

``` sh
builders = ssh://mymachine x86_64-linux - 4; ... more
```

Look to (the nixos
manual)[https://nixos.org/nix/manual/#chap-distributed-builds] for 
more.





