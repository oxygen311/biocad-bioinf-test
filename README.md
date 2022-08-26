## biocad-bioinf-test

Implementation of test haskell-bioinf task.

## Usage

Binaries can be compiled and installed with `stack install` command;

To perform tests use `stack test` command.

### `light-main-bin` 

It's implementation of *simple version* of task. 
It uses `PDB` file path as input, parses it using `cobot-io` library, performs calling of atoms in hydrophobic amino acids, and then clusterize them into components based on euclidian distance.
It outputs spacial clusters of hydrophobic atoms.

### `hard-main-bin`

It's implementation of *hard version* of task. 
It uses `PDB` file path as input, parses it using `cobot-io`. And then performs construction of Voronoi diagram based on all atoms from PDB file using `qhull` library to call surface atoms.
Then it filters surface atoms leaving only hydrophobic ones and outputs them.