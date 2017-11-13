#!/bin/bash

VM=./VM

function assertRun()
{
  RUN=$(timeout 5s $VM "$1")

  if [[ $? -eq 124 ]]
  then
    echo "Run timed out for $1. Pass."
    return
  fi

  if [[ "x$RUN" = "x$2" ]] ; then
   echo "Successful test $1 returns $2"
 else
   echo "Unsuccessful test $1 returns $RUN instead of $2"
 fi
}

assertRun "./tests/Arithmetic base/1test.cml" 15
assertRun "./tests/Arithmetic base/2test.cml" 25
assertRun "./tests/Arithmetic base/3test.cml" 100

assertRun "./tests/Flow Control/1test.cml" 50
assertRun "./tests/Flow Control/5test.cml" 1
assertRun "./tests/Flow Control/2test.cml" 11

assertRun "./tests/Functions/2test.cml" 298
assertRun "./tests/Functions/1test.cml" 100

assertRun "./tests/Memory/2test.cml" 10

assertRun "./tests/Programs/fibo.cml" 55
assertRun "./tests/Programs/fibo_rec.cml" 21


