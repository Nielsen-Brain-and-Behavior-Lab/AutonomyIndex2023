#!/bin/bash

HOME=/fslgroup/fslg_spec_networks/compute
code_DIR=${HOME}/code/HCPD_analysis/CBIG2016_preproc
output_DIR=/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/CBIG2016_preproc_FS6

##Change this first line to your code_DIR pointing to the subjids file
#for subj in `cat $code_DIR/subjids/ids.txt`;do
for subj in sub-HCD1221627; do
    mkdir -p ${code_DIR}/logfiles
    sbatch \
    -o ${code_DIR}/logfiles/output_${subj}.txt \
    -e ${code_DIR}/logfiles/error_${subj}.txt \
    ${code_DIR}/preproc_job.sh \
    ${subj}
    sleep 1
done
