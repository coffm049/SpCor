#!/bin/bash
#SBATCH -n 1
#SBATCH --mem 8gb
#SBATCH --time 02:00:00
#SBATCH --job-name=Rpackages

cd /home/rando149/coffm049/tools/SpCor
module load R

Rscript scripts/environment.R