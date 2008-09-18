#!/bin/sh

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
Usage: $name [-b] [-s] [-r]  <emacs> <lisp>"
  -r  show results file
  -s  use screen to hide emacs
  -B  disable batch mode
  -T  no temp directory (use slime in current directory)
EOF
    exit 1
}

name=$0
batch_mode=-batch # command line arg for emacs
use_temp_dir=true

while getopts srBT opt; do
    case $opt in
	s) use_screen=true;;
	r) dump_results=true;;
	B) batch_mode="";;
	T) use_temp_dir=false;;
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

trap "rm -r $tmpdir" EXIT	# remove temporary directory on exit

mkdir $tmpdir
if [ $use_temp_dir == true ] ; then 
    cp -r $slimedir/*.{el,lisp} ChangeLog $slimedir/contrib  $tmpdir
fi

cmd=($emacs -nw -q -no-site-file $batch_mode --no-site-file
       --eval "(setq debug-on-quit t)"
       --eval "(add-to-list 'load-path \"$testdir\")"
       --eval "(require 'slime)"
       --eval "(setq inferior-lisp-program \"$lisp\")"
       --eval "(slime-batch-test \"$results\")")

if [ "$use_screen" = "" ]; then
    "${cmd[@]}"
    echo $? > $statusfile
else 
    session=slime-screen.$$
    screen -S $session -m -D \
	bash -c "\"\$@\"; echo \$? > $statusfile" "" "${cmd[@]}" &
    screenpid=$!
    trap "screen -S $session -X quit" SIGINT
    wait $screenpid
fi

if [ -f "$statusfile" ]; then
    [ "$dump_results" = true ] && cat $results;
    echo $(cat $statusfile) "test(s) failed."
else
    # Tests crashed
    echo crashed
fi

exit $status
