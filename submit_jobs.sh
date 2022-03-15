#!/bin/bash
#SBATCH -N 1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --threads-per-core=1
#SBATCH -t 00:45:00

export OMP_NUM_THREADS=1

module purge
module load 2021
module load R/4.1.0-foss-2021a

cp -r "$HOME"/L1Sim_final "$TMPDIR"
cd "$TMPDIR"/L1Sim_final

echo $SLURM_ARRAY_TASK_ID

Rscript --vanilla Simulation.R $SLURM_ARRAY_TASK_ID

cp -r ./*.RDS "$HOME"/L1Sim_final

