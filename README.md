# Solution of the heat equation. Implicit Euler method.

![logo](doc/pics/logo.jpg)

The solution of the Euler method for calculating the heat equation, the Thomas algorithm is used to calculate the system of algebraic equations.

# Table of content

- [How to install](#how-to-install)
- [Thomas algorythm](#thomas-algorythm)
- [Erlang](#erlang)
- [Vizualization](#vizualization)
- [Dependenses](#dependenses)
- [Benchmarks](#benchmarks)

## How to install

## Thomas algorythm

[Thomas algorytm](https://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm) is implemented using the lists. You can look at the implemetation [here](src/cm_tdma_functions.erl).

## Erlang

Here we use the single-threaded determination of the equation. The correct operation of the programm on Erlang OTP version 21.0 guaranteed.

## Vizualization

For the construction of graphs, the Python language is used. You can find him [here](src/vizualization.py).

## Dependenses

This application requier the following dependencies:
1. Erlang OTP version 21.0 or newer.
2. Python with installed matplotlib.

## Benchmarks