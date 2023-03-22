#!/bin/bash
#SBATCH --nodes=1
#SBATCH --exclusive
#SBATCH --ntasks=105
#SBATCH --partition=thin
#SBATCH --time=20:00:00

export OMP_NUM_THREADS=1

module purge
module load 2021 R/4.1.0-foss-2021a

cp -r "$HOME"/L1Sim_2023 "$TMPDIR"
cd "$TMPDIR"/L1Sim_2023

echo $SLURM_ARRAY_TASK_ID

Rscript --vanilla Simulation.R $SLURM_ARRAY_TASK_ID

cp -r ./*.RDS "$HOME"/L1Sim_2023

