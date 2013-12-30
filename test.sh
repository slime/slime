#!/bin/bash

# Run the SLIME test suite inside screen, saving the results to a file.


# This script's exit status is the number of tests failed. If no tests
# fail then no output is printed. If at least one test fails then a
# one-line summary is printed.

# If something unexpected fails, you might get an exit code like 127
# or 255 instead. Sorry.

# This code has been placed in the Public Domain.  All warranties
# are disclaimed.

function usage () {
    cat <<EOF
Usage: $name [-bsRTSC] [-n <name>] <emacs> <lisp>"
  -b  use batch mode
  -s  use screen to hide emacs
  -R  don't show results file
  -T  no temp directory (use slime in current directory)
  -S  don't execute tests in random order (use default ordering)
  -C  don't test slime-fancy contrib
  -n <name>  run only tests matching name <name>
EOF
    exit 1
}

name=$0
batch_mode="" # command line arg for emacs
dump_results=true
use_temp_dir=true
selector=t
randomize=t
skip_fancy=false

while getopts bsRTSCn: opt; do
    case $opt in
	b) batch_mode="-batch";;
	s) use_screen=true;;
	n) selector="\"$OPTARG\"";;
	S) randomize=nil;;
	R) dump_results=false;;
	T) use_temp_dir=false;;
        C) skip_fancy=true;;
	*) usage;;
    esac
done

shift $((OPTIND - 1))
[ $# = 2 ] || usage

emacs=$1; lisp=$2;

# Move the code into a directory in /tmp, so that we can compile it
# for the current lisp.

slimedir=$(dirname $name)
tmpdir=/tmp/slime-test.$$
if [ $use_temp_dir == true ] ; then
    testdir=$tmpdir
else
    testdir=$(pwd)
fi
results=$tmpdir/results
statusfile=$tmpdir/status

test -d $tmpdir && rm -r $tmpdir

mkdir $tmpdir
if [ $use_temp_dir == true ] ; then
    cp -r $slimedir/*.{el,lisp} ChangeLog $tmpdir
    if [ $skip_fancy == false ]; then
        mkdir $tmpdir/contrib
        cp -r $slimedir/contrib/*.{el,lisp} $tmpdir/contrib
    fi
fi

if [ $skip_fancy == false ]; then
    contribs="'(slime-fancy)"
fi

cmd=($emacs -nw -q -no-site-file $batch_mode --no-site-file
       --eval "(setq debug-on-quit t)"
       --eval "(add-to-list 'load-path \"$testdir\")"
       --eval "(add-to-list 'load-path \"$testdir/contrib\")"
       --eval "(require 'slime-tests)"
       --eval "(slime-setup $contribs)"
       --eval "(setq inferior-lisp-program \"$lisp\")"
       --eval "(slime-batch-test \"$results\" $selector $randomize)")

if [ "$use_screen" = "" ]; then
    "${cmd[@]}"
    echo $? > $statusfile
else
    session=slime-screen.$$
    screen -S $session -m -D \
	bash -c "\"\$@\"; echo \$? > $statusfile" "" "${cmd[@]}" &
    screenpid=$!
    trap "screen -S $session -X quit" SIGINT SIGQUIT
    wait $screenpid
fi

if [ -f "$statusfile" ]; then
    [ "$dump_results" = true ] && cat $results
    status=$(cat $statusfile)
    echo -n $status "test(s) failed."
else
    # Tests crashed
    echo -n crashed
    status=255
fi

if [ $use_temp_dir == true ]; then
    if [ $status -eq 0 ]; then
        echo -n " Removing temp dir $tmpdir."
        rm -rf $tmpdir
    else
        echo -n " Keeping temp dir $tmpdir."
    fi
fi
echo

exit $status
