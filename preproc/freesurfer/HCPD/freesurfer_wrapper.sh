#!/bin/bash

genDir=/fslgroup/fslg_spec_networks/compute
codeDir=${genDir}/code/HCPD_analysis/freesurfer
dataDir=${genDir}/data/HCPD_BIDS

for subj in `cat ${codeDir}/subjids.txt`; do
    mkdir -p ${codeDir}/logfiles
    sbatch \
        -o ${codeDir}/logfiles/output_sub-${subj}.txt \
        -e ${codeDir}/logfiles/error_sub-${subj}.txt \
        ${codeDir}/freesurfer_job.sh \
        ${subj}
        sleep 1
done
