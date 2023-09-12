# CBIG2016 fMRI Preprocessing Implementation for the HCP Dataset

## Description
The following scripts were used to implement the single-echo fMRI preprocessing pipeline (Note: Actual pipeline code is available at the CBIG GitHub repo: https://github.com/ThomasYeoLab/CBIG/tree/master/stable_projects/preprocessing/CBIG_fMRI_Preproc2016).

## Contents

1. 4SESS_IDS.txt and HCP_replication_set_230608.txt Lists of anonymized subject IDs.
2. example_config.txt Sets the preprocessing steps and desired order.
3. get_individual_subjects.sh Necessary to generate text file with paths for each subjects' mutli-echo runs
4. preproc_wrap.sh Wrapper script to run the preprocessing pipeline. Tied to preproc_job.sh and submits one job per subject.
5. preproc_job.sh Job script to run the preprocessing pipeline.
6. CBIG_preproc_tested_config_funconn.sh This is a configuration script provided by CBIG. This script set the $CBIG_CODE_DIR variable in addition to paths to software.

#Tutorial Guide
A tutorial guide to using this preprocessing pipeline is available on NeuroDocs: https://neurodocs.readthedocs.io/en/latest/cprep/cprep_ov.html
