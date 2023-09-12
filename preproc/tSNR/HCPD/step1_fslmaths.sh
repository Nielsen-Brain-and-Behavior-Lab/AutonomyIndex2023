#!/bin/bash

#Purpose: Create SNR maps using fslmaths.
#Input: CBIG2016 preproc output.
#Output: SNR maps. 
#Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License 2023

#PATHS:
CODE_DIR=/fslgroup/fslg_spec_networks/compute/code/HCPD_analysis/tSNR/ALL
PREPROC_DIR=/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/CBIG2016_preproc_FS6_REST
OUT_DIR=/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/parc_output_fs6_HCPD_REST/quant_metrics/tSNR
mkdir -p ${OUT_DIR}

#LOOP
for sub in `cat ${CODE_DIR}/ids.txt`; do
	for sess in {1..6}; do
			BOLD_FILE="${PREPROC_DIR}/sub-${sub}/sub-${sub}/bold/00${sess}/sub-${sub}_bld00${sess}_rest_skip4_mc.nii.gz"
			T_FILE="${OUT_DIR}/sub-${sub}_sess-${sess}_PREPROC_TSNR.nii.gz"
			MEAN_FILE=${OUT_DIR}/sub-${sub}_sess-${sess}_mean.nii.gz
			STD_FILE=${OUT_DIR}/sub-${sub}_sess-${sess}_std.nii.gz
			fslmaths ${BOLD_FILE} -Tmean ${MEAN_FILE}
			fslmaths ${BOLD_FILE} -Tstd ${STD_FILE}
			fslmaths ${MEAN_FILE} -div ${STD_FILE} ${T_FILE}
			rm ${MEAN_FILE}
			rm ${STD_FILE}	
	done
done
