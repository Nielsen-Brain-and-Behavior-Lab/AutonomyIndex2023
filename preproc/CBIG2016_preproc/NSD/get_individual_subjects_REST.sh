#!/bin/bash

# Set your paths here
HOME=/fslgroup/fslg_spec_networks/compute
code_DIR=${HOME}/code/NSD_analysis/CBIG2016_preproc
data_DIR=/fslgroup/fslg_fun_conn/compute/NSD_analysis/NSD_BIDS

# Create that special CBIG subjids text file for each subject (data must be in BIDS format)
# CBIG example format: 001 /fslhome/NETID/Downloads/CBIG_Data/Sub0001/func/Sub0001_Ses1.nii
# Ideal for running subjects parallel
# Change the path of the first for loop to be your code_DIR

counter=0
for subj in `cat $code_DIR/subjids/subjids.txt`; do
	for SES in ${data_DIR}/sub-${subj}/ses-nsd*/; do
		ses=`basename "$SES"`		
		for i in ${data_DIR}/sub-${subj}/${ses}/func/sub-${subj}_${ses}_task-rest_run-*_bold.nii.gz; do
			if [ -f "${i}" ]; then
			counter=$((counter+1))
			(echo 00$counter ${i}) >> ${data_DIR}/sub-${subj}/sub-${subj}_REST_fmrinii.txt
			else
			echo "${i}"
			fi
		done
	done
counter=0
done
