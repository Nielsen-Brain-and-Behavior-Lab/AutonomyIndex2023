#!/bin/bash

HOME=/fslgroup/fslg_spec_networks/compute
code_DIR=${HOME}/code/NSD_analysis/CBIG2016_preproc
output_DIR=/fslgroup/fslg_fun_conn/compute/NSD_analysis/CBIG2016_preproc_FS6_REST
mkdir -p $output_DIR

##Change this first line to your code_DIR pointing to the subjids file
#for subj in `cat $code_DIR/subjids/subjids.txt`;do
for subj in 01; do
    mkdir -p ${code_DIR}/logfiles
    sbatch \
    -o ${code_DIR}/logfiles/output_${subj}.txt \
    -e ${code_DIR}/logfiles/error_${subj}.txt \
    ${code_DIR}/preproc_job_REST.sh \
    ${subj}
    sleep 2
done
