# Functional Connectivity Matrices (NSD dataset)

## Description
After preprocessing, a functional connectivity matrix was generated for each run within each participant. These matrices were then averaged within each participant. Following the generation of average FC matrices, the autonomy index could be calculated.

## Contents
1. Generate single FC matrices: subjids.txt Text file of anonymized subject IDs for the 232 HCP subjects Generate_FC_text_HCP_ALL.sh Used to generate text files with paths to preprocessed surface-projected output FC_matrix_wrapper.sh Wrapper script to submit jobs for each run for each subject FC_matrix_job.sh Job script for generating FC matrices for each run FC_matrix_SINGLE.m MATLAB template script which is altered for each run from each subject
2. Generate subject-averaged FC matrices: avg_FC_matrix_wrapper.sh Wrapper script used to submit jobs for each subject avg_FC_matrix_job.sh Job script (SLURM) avg_FC_matrix_SINGLE.m MATLAB template script which is altered for each subject
