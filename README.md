# DBPnet: Inferring cooperation of DNA binding proteins in 3D genome.

Please take a look at the [documentation](http://wanglab.ucsd.edu/star/DBPnet/).

## Build from source

[GHC-7.10.2](https://www.haskell.org/ghc/) is required to build DBPnet from source. I suggest use [stack](http://docs.haskellstack.org/en/stable/README.html) to install GHC and other DBPnet dependent packages.

### Install stack

Download the latest release of stack for your platform: https://github.com/commercialhaskell/stack/releases. For example, if your system is CentOS 6.5, the "Linux 64-bit, libgmp4 for CentOS 6.x" is what you need.

### Download DBPnet source and install GHC

1. Download the source code of DBPnet and unpack it.
```
tar zxf DBPnet-vX.X.X.tar.gz
```
2. (Make sure `stack` is in your system path by setting `PATH` variable) Go into the source code directory and install GHC.
```
cd DBPnet-vX.X.X
stack setup
```
To test whether GHC installation is successful, type `stack ghci`. If everything goes well, you would see a GHCi prompt. If it complaints gmp library is missing, make sure appropriate gmp library is in your dynamic linking path (LD_LIBRARY_PATH).

3. Once you have a working copy of GHC, you can proceed to install the dependencies of DBPnet. Under the source code directory, type:
```
stack build --only-dependencies
```

4. Install DBPnet
```
stack install
```

## How to run DBPnet

Make sure DBPnet executable is in your system path. Type `DBPnet --help` to see available command line options.

### Preparing input data for DBPnet

#### ChIP-seq samples

DBPnet analyzes aligned tag files in BED format to create the network. To start, write down all samples in a new [yaml](http://www.yaml.org/start.html) file. Here is a example:

```
example.yaml
------------
- target: CTCF
  files:
      - path: path-to-data/CTCF.bed.gz
      - format: BedGZip

- target: RAD21
  files:
      - path: path-to-data/RAD21.bed
      - format: Bed
```

NOTE: all fields are case sensitive. If the format is not specified, DBPnet will try to guess the correct format based on the file names.

#### Chromosome long range interactions (Optional)

This is an optional input for DBPnet. It is a 6-field Tab-separated file, and each line consists of two genomic loci that form a loop. For example:

```
chr1 [Tab] 10000 [Tab] 11000 [Tab] chr1 [Tab] 20000 [Tab] 21000
```

### Running DBPnet

* With a loop file:

```
DBPnet samples.yaml --loop loops.tsv --output DBPnet_output
```

The results, including 2D and 3D cooperation, will be saved to directory `DBPnet_output`.

* Without a loop file:

```
DBPnet samples.yaml --output DBPnet_output
```

The results, only 2D cooperation, will be saved to directory `DBPnet_output`.
