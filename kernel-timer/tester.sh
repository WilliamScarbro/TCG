#!/bin/bash

if [[ -z $1 ]]; then
    echo "Usage: tester.sh file-name"
    exit
fi

pushd "/home/scarbro/CSU/TCG/kernel-timer" > /dev/null

outfile=/tmp/kernel-test-results
rm $1
make > /dev/null
rm $outfile
$1  > $outfile 
if [[ "$?" != 0 ]]; then
    echo "failed"
else
    cat $outfile | grep result
fi

popd > /dev/null
