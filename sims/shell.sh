#!/bin/bash
ml fhR
bash R --no-save --args "${@:2}" < $1

