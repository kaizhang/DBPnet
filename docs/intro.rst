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
          - format: BedGZip

    - target: RAD21
      files:
          - path: path-to-data/RAD21.bed
          - format: Bed

NOTE: all fields are case sensitive. If the format is not specified,
DBPnet will try to guess the correct format based on the file names.

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

    DBPnet samples.yaml --loop loops.tsv --output DBPnet_output

The results, including 2D and 3D cooperation, will be saved to directory
``DBPnet_output``.

-  Without a loop file:

::

    DBPnet samples.yaml --output DBPnet_output

The results, only 2D cooperation, will be saved to directory
``DBPnet_output``.

An example data set is available in :doc:`demo`.
