#!/bin/sh

# This code has been placed in the Public Domain.  All warranties
# are disclaimed.

version="1.2"
dist="slime-$version"

if [ -d $dist ]; then rm -rf $dist; fi

mkdir -p $dist/contrib
cp NEWS README HACKING PROBLEMS ChangeLog *.el *.lisp $dist/
cp -r contrib/* $dist/contrib

mkdir $dist/doc
cp doc/Makefile doc/slime.texi doc/texinfo-tabulate.awk $dist/doc

tar czf $dist.tar.gz $dist
