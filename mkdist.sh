#!/bin/sh
version="1.0alpha0"
dist="slime-$version"

if [ -d $dist ]; then rm -rf $dist; fi

mkdir $dist
cp NEWS README HACKING ChangeLog *.el *.lisp $dist/

mkdir $dist/doc
cp doc/Makefile doc/slime.texi doc/texinfo-tabulate.awk $dist/doc

tar czf $dist.tar.gz $dist
