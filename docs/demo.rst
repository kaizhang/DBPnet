Demo
====

1. First download the `example <http://wanglab.ucsd.edu/star/DBPnet/example.tar.gz>`_.

2. Untar the data set.

::

    tar zxf example.tar.gz
    cd example

3. Run DBPnet.

::

    path-to-DBPnet/DBPnet input.yaml -l loops.txt --chrom_size hg19

4. The program will take ~ 1 minute to finish, and results are in the ``DBPnet_output`` directory.
