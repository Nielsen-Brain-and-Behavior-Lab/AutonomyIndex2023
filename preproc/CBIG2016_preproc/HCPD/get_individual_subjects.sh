#!/bin/bash

# Set your paths here
HOME=/fslgroup/fslg_spec_networks/compute
code_DIR=${HOME}/code/HCPD_analysis/CBIG2016_preproc
data_DIR=${HOME}/data/HCPD_BIDS

# Create that special CBIG subjids text file for each subject (data must be in BIDS format)
# CBIG example format: 001 /fslhome/NETID/Downloads/CBIG_Data/Sub0001/func/Sub0001_Ses1.nii
# Ideal for running subjects parallel
# Change the path of the first for loop to be your code_DIR

counter=0
for x in `ls $data_DIR`; do 	
	subj=`basename $x`
	for i in ${data_DIR}/${subj}/ses-1/func/${subj}_ses-1_task-rest_run-*_bold.nii.gz; do
		counter=$((counter+1))
		(echo 00$counter ${i}) >> ${data_DIR}/${subj}/${subj}_fmrinii.txt
	done
counter=0
done
