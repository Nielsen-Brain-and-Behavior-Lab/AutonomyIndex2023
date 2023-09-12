#!/bin/bash

# Set your paths here
HOME=/fslgroup/fslg_spec_networks/compute
code_DIR=${HOME}/code/HCP_analysis/CBIG2016_preproc
data_DIR=/fslgroup/grp_proc/compute/HCP_analysis/HCP_download

# Create that special CBIG subjids text file for each subject (data must be in BIDS format)
# CBIG example format: 001 /fslhome/NETID/Downloads/CBIG_Data/Sub0001/func/Sub0001_Ses1.nii
# Ideal for running subjects parallel
# Change the path of the first for loop to be your code_DIR

counter=0
for subj in `cat $code_DIR/subjids/subjids.txt`; do 	
	for i in ${data_DIR}/${subj}/func/sub-${subj}_*.nii.gz; do
		counter=$((counter+1))
		(echo 00$counter ${i}) >> ${data_DIR}/${subj}/sub-${subj}_fmrinii.txt
	done
counter=0
done
