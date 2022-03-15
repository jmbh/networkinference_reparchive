#!/bin/bash

mkdir "$TMPDIR"/L1Sim_final/

cd "$HOME"/L1Sim_final

sbatch -a 1-100 submit_jobs.sh




