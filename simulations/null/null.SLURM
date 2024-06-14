#!/bin/bash
#SBATCH --array 1-100
#SBATCH -n 8
#SBATCH --mem 8gb
#SBATCH --time 00:30:00
#SBATCH --job-name=SpCor

cd /home/rando149/coffm049/tools/SpCor
module load R

Rscript scripts/simulate_n_devtools.R --nsites 50 --nfamPerSite 20 --nmembersPerFamily 2 \
  --nfeatures 10 --permutations 1000 --cores 8 --beta 0 --scenario "null_${SLURM_ARRAY_TASK_ID}"
