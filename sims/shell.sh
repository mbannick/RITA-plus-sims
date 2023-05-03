#!/bin/bash
ml fhR
echo "Test"
bash R --no-save --args "${@:2}" < $1

