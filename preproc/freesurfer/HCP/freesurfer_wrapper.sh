#!/bin/bash

genDir=/fslgroup/fslg_spec_networks/compute
codeDir=${genDir}/code/HCP_analysis/freesurfer
dataDir=/fslgroup/grp_proc/compute/HCP_analysis/HCP_download

for subj in `cat ${codeDir}/subjids/subjids.txt`; do
    mkdir -p ${codeDir}/logfiles
    sbatch \
        -o ${codeDir}/logfiles/output_sub-${subj}.txt \
        -e ${codeDir}/logfiles/error_sub-${subj}.txt \
        ${codeDir}/freesurfer_job.sh \
        ${subj}
        sleep 1
done
