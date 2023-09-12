#!/bin/bash

#Purpose: Rename surface-projected preproc output as consecutive sessionsand copy to new location so as to align with 12 consecutive runs selected for FC Matrices
#Inputs: CBIG2016 preproc output
#Outputs: Consecutively named sessions.

#PATHS
PREP_DIR=/fslgroup/fslg_fun_conn/compute/NSD_analysis/CBIG2016_preproc_FS6_REST
CODE_DIR=/fslgroup/fslg_spec_networks/compute/code/NSD_analysis/Kong2019_parc_all_REST
NEW_DIR=/fslgroup/fslg_fun_conn/compute/NSD_analysis/CBIG2016_preproc_FS6_REST_CONSEC

mkdir -p $NEW_DIR

#LOOP
for SUB in `cat $CODE_DIR/ids.txt`; do
	count=0
	mkdir -p ${NEW_DIR}/${SUB}/${SUB}/bold/mc $NEW_DIR/${SUB}/${SUB}/qc
	
	for SES in {1..25}; do
			PAD_SES=`printf %03d $SES`
			FD_FILE=${PREP_DIR}/${SUB}/${SUB}/bold/mc/${SUB}_bld${PAD_SES}_rest_skip4_motion_outliers_FDRMS
			DVARS_FILE=${PREP_DIR}/${SUB}/${SUB}/bold/mc/${SUB}_bld${PAD_SES}_rest_skip4_motion_outliers_DVARS
			VOLS_FILE=${PREP_DIR}/${SUB}/${SUB}/qc/${SUB}_bld${PAD_SES}_FDRMS0.2_DVARS50_motion_outliers.txt
			RH_FILE=${PREP_DIR}/${SUB}/${SUB}/surf/rh.${SUB}_bld${PAD_SES}_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz

			if [ -f "${RH_FILE}" ] && [ "${count}" -lt 12 ]; then
				count=$((count+1))
				pad_count=`printf %03d $count`	
				cp ${FD_FILE} ${NEW_DIR}/${SUB}/${SUB}/bold/mc/${SUB}_bld${pad_count}_rest_skip4_motion_outliers_FDRMS
				cp ${DVARS_FILE} ${NEW_DIR}/${SUB}/${SUB}/bold/mc/${SUB}_bld${pad_count}_rest_skip4_motion_outliers_DVARS
				cp ${VOLS_FILE} ${NEW_DIR}/${SUB}/${SUB}/qc/${SUB}_bld${pad_count}_FDRMS0.2_DVARS50_motion_outliers.txt
			else
				echo ""
			fi
	done
done 

