#!/bin/bash

#SBATCH --time=1:00:00   # walltime
#SBATCH --ntasks=1   # number of processor cores (i.e. tasks)
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=405000M   # memory per CPU core
#SBATCH -J "ind_AI"   # job name


# Set the max number of threads to use for programs using OpenMP. Should be <= ppn. Does nothing if the program doesn't use OpenMP.
export OMP_NUM_THREADS=$SLURM_CPUS_ON_NODE



# LOAD MODULES, INSERT CODE, AND RUN YOUR PROGRAMS HERE
#load CBIG environment
source /fslgroup/fslg_rdoc/compute/test_scripts/parc_scripts/CBIG_preproc_tested_config_funconn.sh

#load matlab
module load matlab/r2018b
LD_PRELOAD= matlab &
unset DISPLAY

#go to subject code dir with individual matlab script
SUBJ_CODE_DIR=/fslgroup/fslg_spec_networks/compute/code/HCPD_analysis/ai_spec/REST/individual_ai/subj_scripts/${SUB}
cd $SUBJ_CODE_DIR

#call matlab to run the AI_matrix_SINGLE.m script
matlab -nodisplay -nojvm -nosplash -r ai_ALL_SINGLE

