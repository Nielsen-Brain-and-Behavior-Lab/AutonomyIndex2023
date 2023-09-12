#!/bin/bash

#Purpose: Create label files in fs6 space - from individual parcellation files
#Inputs: parc2annot annotation files
#Outputs: .label.gii parcellation files
#Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License 2022

#SET PATHS
HOME=/fslgroup/fslg_spec_networks/compute
CODE_DIR=${HOME}/code/NSD_analysis/Kong2019_parc_all_REST
HOME_DIR=/fslgroup/fslg_fun_conn/compute/NSD_analysis
REST1=${HOME_DIR}/parc_output_fs6_NSD_ALL_REST/quant_metrics/MSHBM_vis
REST2=${HOME_DIR}/parc_output_fs6_NSD_EVEN_REST/quant_metrics/MSHBM_vis
REST3=${HOME_DIR}/parc_output_fs6_NSD_ODD_REST/quant_metrics/MSHBM_vis
REST4=${HOME_DIR}/parc_output_fs6_NSD_FIRST_HALF_REST/quant_metrics/MSHBM_vis
REST5=${HOME_DIR}/parc_output_fs6_NSD_SECOND_HALF_REST/quant_metrics/MSHBM_vis
REST6=${HOME_DIR}/parc_output_fs6_NSD_RAND1_1_REST/quant_metrics/MSHBM_vis
REST7=${HOME_DIR}/parc_output_fs6_NSD_RAND1_2_REST/quant_metrics/MSHBM_vis
REST8=${HOME_DIR}/parc_output_fs6_NSD_12_REST/quant_metrics/MSHBM_vis

#.surf.gii files can be created using mris_convert on the $FREESURFER_HOME/subjects/fsaverage6/surf inflated files

#Create .label.gii parcellation files
#for OUTDIR in $REST1 $REST2 $REST3 $REST4 $REST5 $REST6 $REST7; do
for OUTDIR in $REST8; do
count=0
for SUB in `cat $CODE_DIR/subjids.txt`; do
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
done


