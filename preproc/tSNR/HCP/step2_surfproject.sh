#!/bin/bash

#Purpose: Project tSNR maps to fsaverage6 surface.
#Inputs: tSNR maps from Step1, freesurfer native output
#Outputs: .shape.gii (HCP WB compatible) surface SNR maps
#Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License 2022

#SET PATHS
CODE_DIR=/fslgroup/fslg_spec_networks/compute/code/HCP_analysis/tSNR/ALL
PREP_DIR=/fslgroup/fslg_autism_networks/compute/HCP_analysis/CBIG2016_preproc_FS6_ALL
T_DIR=/fslgroup/grp_hcp/compute/HCP_analysis/parc_output_fs6_HCP_ALL/quant_metrics/tSNR
FS_DIR=/fslgroup/grp_proc/compute/HCP_analysis/HCP_download
SUBJECTS_DIR=${FREESURFER_HOME}/subjects

#LOOP THROUGH EACH SUBJECT AND TASK
#for SUBJ in 100307; do
for SUBJ in `cat $CODE_DIR/ids.txt`; do
	for SESS in {1..4}; do
		#BBREGISTER (OBTAIN AFFINE.REG)
		bbregister \
		--bold \
		--s fsaverage6 \
		--init-fsl \
		--mov ${PREP_DIR}/sub-${SUBJ}/sub-${SUBJ}/bold/00${SESS}/sub-${SUBJ}_bld00${SESS}_rest_skip4_mc.nii.gz \
		--reg ${T_DIR}/sub-${SUBJ}_sess-${SESS}_register.dat

		#MRI_VOL2SURF LH 
		mri_vol2surf --mov ${T_DIR}/sub-${SUBJ}_sess-${SESS}_PREPROC_TSNR.nii.gz \
		--reg ${T_DIR}/sub-${SUBJ}_sess-${SESS}_register.dat \
		--hemi lh \
		--projfrac 0.5 \
		--o ${T_DIR}/sub-${SUBJ}_sess-${SESS}_lh.shape.gii \
		--noreshape \
		--interp trilinear

		#MRI_VOL2SURF RH
		mri_vol2surf --mov ${T_DIR}/sub-${SUBJ}_sess-${SESS}_PREPROC_TSNR.nii.gz \
		--reg ${T_DIR}/sub-${SUBJ}_sess-${SESS}_register.dat \
		--hemi rh \
		--projfrac 0.5 \
		--o ${T_DIR}/sub-${SUBJ}_sess-${SESS}_rh.shape.gii \
		--noreshape \
		--interp trilinear

	done
done

