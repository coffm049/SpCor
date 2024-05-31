#!/bin/bash

Rscript scripts/simulate.R --nsites 10 --nfamPerSite 10 --nmembersPerFamily 2 \
  --nfeatures 5 --permutations 10 --cores 4 --beta 0 --scenario "null"
