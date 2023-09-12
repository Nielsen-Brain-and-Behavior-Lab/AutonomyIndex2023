#!/bin/bash

#Purpose: submit jobs to generate individual full-surface FC matrices

#Set paths and vars
HOME=/fslgroup/fslg_spec_networks/compute
CODE_DIR=${HOME}/code/NSD_analysis/FC_matrices/REST/ALL
HOME_D=/fslgroup/fslg_fun_conn/compute
PREP_DIR=${HOME_D}/NSD_analysis/CBIG2016_preproc_FS6_REST_CONSEC

#Submit a job for each sub/sess
count=0
for sub in `cat ${CODE_DIR}/subjids/subjids.txt`; do
	SUB=`basename "$sub"`
	for SES in {1..12}; do
		if [ "${SES}" -lt 10 ]; then
	    		FILE=$PREP_DIR/${SUB}/${SUB}/surf/lh.${SUB}_bld00${SES}_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz
	    	else
			FILE=$PREP_DIR/${SUB}/${SUB}/surf/lh.${SUB}_bld0${SES}_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz
		fi
	        #if file exists
       		if [ -f "$FILE" ]; then
			count=$((count+1))
 		        if [ "$count" -lt 13 ]; then  
				#make new matlab and job scripts for each sub/sess
				CODE_DIR2=${CODE_DIR}/subj_scripts/${SUB}/ses-${SES}
				mkdir -p ${CODE_DIR2}
		
				#matlab script
				matfile=${CODE_DIR}/FC_matrix_SINGLE.m
				cp ${matfile} ${CODE_DIR2}
		
				sed -i 's|SUB|'"${SUB}"'|g' ${CODE_DIR2}/FC_matrix_SINGLE.m
				sed -i 's|SES|'"${SES}"'|g' ${CODE_DIR2}/FC_matrix_SINGLE.m
	
				#job script
				jobfile=${CODE_DIR}/FC_matrix_job.sh
				cp ${jobfile} ${CODE_DIR2}
	
				sed -i 's|${SUB}|'"${SUB}"'|g' ${CODE_DIR2}/FC_matrix_job.sh
				sed -i 's|${SES}|'"${SES}"'|g' ${CODE_DIR2}/FC_matrix_job.sh

				#submit job 
				sbatch ${CODE_DIR2}/FC_matrix_job.sh
			else
				echo "no $SES $SUB"
			fi
		else
			echo " "
fi
done
count=0
done
