#!/bin/bash

#SBATCH -o logs/run.log-%j
#SBATCH --exclusive

# Rscript analysis_2p1.R
Rscript analysis_2p3-results_3p2.R
