#!/bin/sh

# Run the SLIME test suite in batch mode, saving the results to a file.

# This script's exit status is the number of tests failed. If no tests
# fail then no output is printed. If at least one test fails then a
# one-line summary is printed.

# If something unexpected fails, you might get an exit code like 127
# or 255 instead. Sorry.

if [ $# != 4 ]; then
    echo "Usage: $0 <emacs> <lisp> <dribble-file> <results-file>"
    exit 1
fi

emacs=$1; lisp=$2; dribble=$3; results=$4
slimedir=$(dirname $0)

# Move the code into a directory in /tmp, so that we can compile it
# for the current lisp.

testdir=/tmp/slime-test.$$
test -d $testdir && rm -r $testdir
trap "rm -r $testdir" EXIT	# remove temporary directory on exit

mkdir $testdir
cp $slimedir/*.el $slimedir/*.lisp $testdir

# you can remove "--batch" to get an emacs window for troubleshooting.
$emacs --batch --no-site-file --no-init-file \
       --eval "(setq debug-on-quit t)" \
       --eval "(setq max-lisp-eval-depth 1000)" \
       --eval "(setq load-path (cons \"$testdir\" load-path))" \
       --eval "(require 'slime)" \
       --eval "(setq inferior-lisp-program \"$lisp\")" \
       --eval "(slime-batch-test \"${results}\")" \
       &> $dribble \

status=$?

if [ $status != 0 ]; then
    echo $status "test(s) failed."
fi

exit $status

