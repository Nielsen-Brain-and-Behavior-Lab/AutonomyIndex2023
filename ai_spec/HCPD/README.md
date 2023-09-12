# Autonomy Index (HCPD dataset)

## Description
The scripts in the folder are used to calculate the autonomy index on each vertex of a subject-averaged functional connectivity matrix. Then, autonomy index values are averaged within network boundaries (separate for the right and left hemispheres). Individual parcellations are used to delineate network boundaries (MS-HBM Kong2019).

## Contents
1. Generate AI matrices

    * ai_ALL_wrapper.sh Wrapper script used to submit jobs for each subject
    * ai_ALL_job.sh Job script
    * ai_ALL_SINGLE.m MATLAB template script that is updated for each subject
    * subjids.txt Text file containing anonymized subject IDs (HCP 232)
    * ai2gii_ALL.m Converts AI .mat files to GIFTI format for HCP Workbench viewing

2. Average AI within individual parcellation network boundaries
    * INDIVIDUAL_avg_ai_MSHBM_HCPD_ALL.py Python (v3.6) script used to average autonomy index values within boundaries of individual parcellations
