#!/bin/bash

#SBATCH --time=45:00:00   # walltime
#SBATCH --ntasks=1   # number of processor cores (i.e. tasks)
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=16384M  # memory per CPU core
#SBATCH -J "utah_FS"   # job name


# Compatibility variables for PBS. Delete if not needed.
export PBS_NODEFILE=`/fslapps/fslutils/generate_pbs_nodefile`
export PBS_JOBID=$SLURM_JOB_ID
export PBS_O_WORKDIR="$SLURM_SUBMIT_DIR"
export PBS_QUEUE=batch

# Set the max number of threads to use for programs using OpenMP.
export OMP_NUM_THREADS=$SLURM_CPUS_ON_NODE

# LOAD ENVIRONMENTAL VARIABLES
export FREESURFER_HOME=/fslhome/mpeter55/compute/research_bin/freesurfer
source $FREESURFER_HOME/SetUpFreeSurfer.sh

# INSERT CODE, AND RUN YOUR PROGRAMS HERE
genDir=/fslgroup/fslg_spec_networks/compute
codeDir=${genDir}/code/HCP_analysis/freesurfer
dataDir=/fslgroup/grp_proc/compute/HCP_analysis/HCP_download

~/compute/research_bin/freesurfer/bin/recon-all \
-subjid sub-${1} \
-i ${dataDir}/${1}/anat/sub-${1}_ses-1_T1w.nii.gz \
-wsatlas \
-all \
-sd ${dataDir}/${1}/anat
