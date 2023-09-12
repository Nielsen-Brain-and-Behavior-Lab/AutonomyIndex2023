# Deconstructed Autonomy Index (HCP Dataset)

## Description
The purpose of this novel measure is to identify which connections contribute to the specialization of a network. As with the autonomy index, this measure requires functional connectivity matrices as input. Then, for each target network (1-17) and each seed vertex of the FC matrix, the degree of within- and corss-hemisphere connectivity is computed. To do this, we sum the number of highly correlated vertices belonging to that target network in each hemisphere. This is normalized by the total number of vertices with a given target network label in each hemisphere. 

From here, you have a matrix of dAI values for each target network for each subject. Then, for each target network matrix, the dAI is averaged within the boundaries of each network within each subject (using an individual parcellation, MS-HBM). 

## Contents
1. Generate dAI matrices for each network, for each subjects
    * subjids.txt Text file of anonymized subject IDs
    * network_level_ai_wrapper.sh Used to submit a job for each subject
    * network_level_ai_job.sh Job script for SLURM/sbatch systems
    * network_level_ai.m Template MATLAB script that is altered for each participant

2. Average across matrices
    * avg_network_level_ai_HCP_ALL.py Python (v3.6) script to average dAI values within individual parcellation network boundaries.
