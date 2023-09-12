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
	mkdir -p ${NEW_DIR}/${SUB}/${SUB}/surf
	for SES in {1..25}; do
			PAD_SES=`printf %03d $SES`
			RH_FILE=${PREP_DIR}/${SUB}/${SUB}/surf/rh.${SUB}_bld${PAD_SES}_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz
			LH_FILE=${PREP_DIR}/${SUB}/${SUB}/surf/lh.${SUB}_bld${PAD_SES}_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz
			if [ -f "${RH_FILE}" ]; then
				count=$((count+1))
				pad_count=`printf %03d $count`	
				cp ${RH_FILE} ${NEW_DIR}/${SUB}/${SUB}/surf/rh.${SUB}_bld${pad_count}_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz
				cp ${LH_FILE} ${NEW_DIR}/${SUB}/${SUB}/surf/lh.${SUB}_bld${pad_count}_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz
			else
				echo ""
			fi
	done
done 

