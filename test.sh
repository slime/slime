#!/bin/bash

# Run the SLIME test suite

# This script's exit status is the number of tests failed. The script
# print detailed test output to stderr and a one-line description of
# the number of failed tests to stdout.

# If something unexpected fails, you might get an exit code like 127
# or 255 instead. Sorry.

# This code has been placed in the Public Domain.  All warranties
# are disclaimed.

function usage () {
    cat <<EOF
Usage: $name [-bsTSCL] [-n <name>] <emacs> <lisp>"
  -b  use batch mode
  -T  no temp directory (use slime in current directory)
  -S  don't execute tests in random order (use default ordering)
  -C  don't test slime-fancy contrib
  -L  load slime instead of byte-compiling
  -n <name>  run only tests matching name <name>
EOF
    exit 1
}

name=$0
batch_mode="" # command line arg for emacs
use_temp_dir=true
selector=t
randomize=t
skip_fancy=false
dont_byte_compile=false

while getopts bTSCLn: opt; do
    case $opt in
	b) batch_mode="-batch";;
	n) selector="\"$OPTARG\"";;
	S) randomize=nil;;
	T) use_temp_dir=false;;
        C) skip_fancy=true;;
        L) dont_byte_compile=true;;
	*) usage;;
    esac
done

shift $((OPTIND - 1))
[ $# = 2 ] || usage

emacs=$1; lisp=$2;

# Move the code into a directory in /tmp, so that we can compile it
# for the current lisp.

slimedir=$(dirname $name)
compiledir=$slimedir
tmpdir=/tmp/slime-test.$$
if [ $use_temp_dir == true ] ; then
    testdir=$tmpdir
else
    testdir=$(pwd)
fi

test -d $tmpdir && rm -r $tmpdir

mkdir $tmpdir
if [ $use_temp_dir == true ] ; then
    cp -r $slimedir/*.{el,lisp} ChangeLog $tmpdir
    compile_dir=$tmpdir
    if [ $skip_fancy == false ]; then
        mkdir $tmpdir/contrib
        cp -r $slimedir/contrib/*.{el,lisp} $tmpdir/contrib
    fi
fi

if [ $dont_byte_compile == false ]; then
    # slime.el is a special case. It needs to be compile and loaded
    # before everything else.
    find $compile_dir -name '*.el' -not -name "slime.el" |
        xargs -t $emacs -L . -L $compile_dir/contrib --batch -f batch-byte-compile $compile_dir/slime.el
fi

if [ $skip_fancy == false ]; then
    contribs="'(slime-fancy)"
fi

cmd=($emacs -nw -q -no-site-file $batch_mode --no-site-file
       --eval "(add-to-list 'load-path \"$testdir\" 'append)"
       --eval "(add-to-list 'load-path \"$testdir/contrib\" 'append)"
       --eval "(require 'slime-tests)"
       --eval "(slime-setup $contribs)"
       --eval "(setq inferior-lisp-program \"$lisp\")"
       --eval "(slime-batch-test $selector $randomize)")

"${cmd[@]}"

status=$?

if [ $use_temp_dir == true ]; then
    if [ $status -eq 0 ]; then
        echo "Success.  Removing temp dir $tmpdir."
        rm -rf $tmpdir
    else
        echo "Failure. Keeping temp dir $tmpdir."
    fi
fi

exit $status
