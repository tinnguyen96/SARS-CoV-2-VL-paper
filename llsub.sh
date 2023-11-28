#!/bin/bash

#SBATCH -o logs/run.log-%j
#SBATCH --exclusive

Rscript analysis_2p1.R
