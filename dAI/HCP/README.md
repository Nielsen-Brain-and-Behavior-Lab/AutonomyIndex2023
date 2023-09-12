# Deconstructed Autonomy Index (HCP Dataset)

## Description
To address the aim of identifying contributions to network specialization, the autonomy index was deconstructed for each network. This was accomplished by first calculating functional connectivity matrices for each BOLD run as previously described. Then, for each target network (1-17) and each seed vertex derived from a functional connectivity matrix, the degree of within- and cross-hemisphere connectivity was computed by summing the number of highly correlated vertices belonging to that target network in each hemisphere. This is normalized by the total number of vertices with a given target network label in each hemisphere. This results in a matrix of dAI values for each target network for each subject. Then, for each target network matrix, the deconstructed AI is averaged within the boundaries of each network within each subject.

## Contents
1. Generate dAI matrices for each network, for each subjects
    * subjids.txt Text file of anonymized subject IDs
    * network_level_ai_wrapper.sh Used to submit a job for each subject
    * network_level_ai_job.sh Job script for SLURM/sbatch systems
    * network_level_ai.m Template MATLAB script that is altered for each participant

2. Average across matrices
    * avg_network_level_ai_HCP_ALL.py Python (v3.6) script to create average matrices per network (averaging across participants)
