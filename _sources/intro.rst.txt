Introduction
============

How to install
--------------

see :doc:`install` or :doc:`build`.

.. _quick-start:

Quick start guide
-----------------

Make sure DBPnet executable is in your system path. Type
``DBPnet --help`` to see available command line options.

Preparing input data for DBPnet
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ChIP-seq samples
^^^^^^^^^^^^^^^^

DBPnet analyzes aligned tag files in BED format to create the network.
To start, write down all samples in a new
`yaml <http://www.yaml.org/start.html>`__ file. Here is a example:

::

    example.yaml
    ------------
    - target: CTCF
      files:
          - path: path-to-data/CTCF.bed.gz
            format: BedGZip

    - target: RAD21
      files:
          - path: path-to-data/RAD21_rep1.bed
            format: Bed
          - path: path-to-data/RAD21_rep2.bed

.. note::

    All fields are case sensitive. If the format is not specified, DBPnet will try to guess the correct format based on the file names.

Chromosome size file
^^^^^^^^^^^^^^^^^^^^

This is a file describing the sizes of each chromosome. Example:

::

    chr1 [Tab] 249250621
    chr2 [Tab] 243199373

You need to provide this file to DBPnet through the command line option ``--chrom_size <FILENAME>``.
For your convenience, DBPnet includes some common genome size (hg19 and mm10). You can
use them by ``--chrom_size hg19``.

Chromosome long range interactions (Optional)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This is an optional input for DBPnet. It is a 6-field Tab-separated
file, and each line consists of two genomic loci that form a loop. For
example:

::

    chr1 [Tab] 10000 [Tab] 11000 [Tab] chr1 [Tab] 20000 [Tab] 21000

Running DBPnet
~~~~~~~~~~~~~~

-  With a loop file:

::

    DBPnet samples.yaml --loop loops.tsv --output DBPnet_output --chrom_size hg19

The results, including 2D and 3D cooperation, will be saved to directory
``DBPnet_output``.

-  Without a loop file:

::

    DBPnet samples.yaml --output DBPnet_output --chrom_size hg19

The results, only 2D cooperation, will be saved to directory
``DBPnet_output``.

An example data set is available in :doc:`demo`.
