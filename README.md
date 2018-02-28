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
NumPieces
NumBlocks Block1 Block2
NumBlocks Block3 Block4 Block5
etc
~~~~

Note: The parser does not work on coordinates that are more than 1 digit.
        i.e. (19,2)

Example:

~~~~
2 4
2
4  0 0  1 0  0 1  1 1
4  0 0  1 0  0 1  1 1
~~~~

For more examples see the input folder.
