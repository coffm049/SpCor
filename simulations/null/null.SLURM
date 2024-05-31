#!/bin/bash

date
Rscript scripts/simulate.R --nsites 50 --nfamPerSite 20 --nmembersPerFamily 2 \
  --nfeatures 10 --permutations 100 --cores 8 --beta 0 --scenario "null"
date
