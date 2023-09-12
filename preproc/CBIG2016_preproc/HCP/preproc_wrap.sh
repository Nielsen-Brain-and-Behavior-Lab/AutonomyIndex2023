#!/bin/bash

HOME=/fslgroup/fslg_spec_networks/compute
code_DIR=${HOME}/code/HCP_analysis/CBIG2016_preproc
output_DIR=/fslgroup/grp_spec/compute/HCP_analysis/CBIG2016_preproc_FS6
output_DIR2=/fslgroup/fslg_HBN_preproc/compute/HCP_analysis/CBIG2016_preproc_FS6
output_DIR3=/fslgroup/fslg_autism_networks/compute/HCP_analysis/CBIG2016_preproc_FS6
output_Dir4=/fslgroup/grp_hcp/compute/HCP_analysis/CBIG2016_preproc_FS6
output_Dir5=/fslgroup/grp_parc/compute/HCP_analysis/CBIG2016_preproc_FS6
output_Dir6=/fslgroup/grp_nsd/compute/HCP_analysis/CBIG2016_preproc_FS6

mkdir -p $output_DIR2

##Change this first line to your code_DIR pointing to the subjids file
#for subj in `cat $code_DIR/subjids/Replication_set_failed_230609.txt`; do
for subj in 201818; do
    mkdir -p ${code_DIR}/logfiles
    sbatch \
    -o ${code_DIR}/logfiles/output_${subj}.txt \
    -e ${code_DIR}/logfiles/error_${subj}.txt \
    ${code_DIR}/preproc_job.sh \
    ${subj}
    sleep 2
done
