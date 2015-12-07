Build from source
=================

`GHC-7.10.2 <https://www.haskell.org/ghc/>`__ is required to build
DBPnet from source. I recommend use
`stack <http://docs.haskellstack.org/en/stable/README.html>`__ to
install GHC and other DBPnet dependent packages.

Install stack
-------------

Download the `latest release of
stack <https://github.com/commercialhaskell/stack/releases>`__ for your
platform. For example, if your system is CentOS 6.5, the "Linux 64-bit,
libgmp4 for CentOS 6.x" is what you need.

Next, unpack the tarball and move ``stack`` executable to a directory
that is in your system path, e.g., ``/usr/bin``.

::

    tar zxf stack-x.x.x-linux-x64.tar.gz
    mv stack-x.x.x-linux-x64/stack /usr/bin

Download DBPnet source and install GHC
--------------------------------------

Download the source code of :download:`DBPnet <release/DBPnet-0.1.0.tar.gz>` and
unpack it.

::

    tar zxf DBPnet-X.X.X.tar.gz

Go into the source code directory and install GHC.

::

    cd DBPnet-X.X.X
    stack setup

To test whether GHC installation is successful, type ``stack ghci``. If
everything goes well, you would see a GHCi prompt. If it complaints gmp
library is missing, make sure appropriate gmp library is in your dynamic
linking path (LD\_LIBRARY\_PATH).

Once you have a working copy of GHC, you can proceed to install the
dependencies of DBPnet. Under the source code directory, type:

::

    stack build --only-dependencies

and then install DBPnet:

::

    stack install
