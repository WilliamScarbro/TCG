#!/bin/bash

export TCG_HOME=$PWD
export TIMER_ITERS=100

#export MATCH_CONTEXT="Factor"
#export MATCH_CONTEXT="Permute"
#export MATCH_CONTEXT="Normalize"
export MATCH_CONTEXT="Join"

export COMPILER="DirectMonty"
#export COMPILER="VectorMonty"
