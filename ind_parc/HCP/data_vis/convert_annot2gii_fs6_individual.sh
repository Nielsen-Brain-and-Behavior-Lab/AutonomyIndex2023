#!/bin/bash

#Purpose: Create label files in fs6 space - from individual parcellation files
#Inputs: parc2annot annotation files
#Outputs: .label.gii parcellation files
#Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License 2022

#SET PATHS
HOME=/fslgroup/fslg_spec_networks/compute
HOME_D=/fslgroup/grp_hcp/compute
CODE_DIR=${HOME}/code/HCP_analysis/Kong2019_parc_fs6_ALL
OUTDIR=${HOME_D}/HCP_analysis/parc_output_fs6_HCP_ALL/quant_metrics/MSHBM_vis


#.surf.gii files can be created using mris_convert on the $FREESURFER_HOME/subjects/fsaverage6/surf inflated files

#Create .label.gii parcellation files
count=0
for SUB in `cat $CODE_DIR/subjids/ids.txt`; do
	count=$((count+1))
	#Resample LH annot
	mri_surf2surf --srcsubject fsaverage --sval-annot ${OUTDIR}/sub-${count}_lh.annot --trgsubject fsaverage6 --hemi lh --trgsurfval ${OUTDIR}/sub-${SUB}_fs6_lh --trg_type annot
	#Resample RH annot
	mri_surf2surf --srcsubject fsaverage --sval-annot ${OUTDIR}/sub-${count}_rh.annot --trgsubject fsaverage6 --hemi rh --trgsurfval ${OUTDIR}/sub-${SUB}_fs6_rh --trg_type annot

	#LH label file
	mris_convert --annot ${OUTDIR}/sub-${SUB}_fs6_lh.annot ${FREESURFER_HOME}/subjects/fsaverage6/surf/lh.white ${OUTDIR}/sub-${SUB}_lh.label.gii
	#RH label file
	mris_convert --annot ${OUTDIR}/sub-${SUB}_fs6_rh.annot ${FREESURFER_HOME}/subjects/fsaverage6/surf/rh.white ${OUTDIR}/sub-${SUB}_rh.label.gii
done



