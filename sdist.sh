rm .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/*.tar.gz
rm statistics/.stack-work/dist/x86_64-linux/Cabal-1.22.4.0/*.tar.gz
stack sdist

mkdir sdist_tmp
mv .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/*.tar.gz sdist_tmp
mv statistics/.stack-work/dist/x86_64-linux/Cabal-1.22.4.0/*.tar.gz sdist_tmp


cd sdist_tmp
tar zxf statistics-0.13.2.4.tar.gz
mv statistics-0.13.2.4 statistics
tar zxf DBPnet-*.tar.gz
rm *.tar.gz
mv statistics DBPnet-*
tarball=`ls`
tar czf $tarball.tar.gz $tarball
mv $tarball.tar.gz ../
cd ..
rm -r sdist_tmp
