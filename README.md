# Haskell Polynomino Solver
A program for solving a polynomino tiling problem

## Compiling and Input
To comple:
`ghc nomino.hs`

To run:
`./nomino filename.txt`

Output will be in the terminal showing the final tiling if a solution can be found.

## Input Files
The input file but be in the following format:

~~~~
Rows Columns
  Block1 Block2
  Block3 Block4 Block5
  etc
~~~~
Each x and y must be separated by a single space and each set must be preceded by two spaces.

Note: The parser does not work on coordinates that are more than 1 digit.
        i.e. (19,2)

Examples:

~~~~
3 3
  0 0  1 0  2 0  1 1
  0 0  1 0  0 1  0 2  1 2
~~~~

~~~~
3 5
  0 0  0 1  0 2  0 3  0 4
  0 0  0 1  0 2  0 3  0 4
  0 0  0 1  0 2  0 3  0 4
~~~~
