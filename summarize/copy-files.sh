#!/bin/sh
mkdir /Users/marlena/Documents/FileZilla/xs-recent/enhanced/$1

# scp -r mnorwood@rhino:/home/mnorwood/simulation-runs/rita/$1/results/ /Users/marlena/Documents/FileZilla/xs-recent/enhanced/$1/

scp mnorwood@rhino:/home/mnorwood/simulation-runs/rita/$1/params.csv /Users/marlena/Documents/FileZilla/xs-recent/enhanced/$1/

scp mnorwood@rhino:/home/mnorwood/simulation-runs/rita/$1/summary.csv /Users/marlena/Documents/FileZilla/xs-recent/enhanced/$1/
