#!/bin/bash

#Purpose: submit jobs to generate individual full-surface FC matrices

#Set paths and vars
HOME=/fslgroup/fslg_spec_networks/compute
CODE_DIR=${HOME}/code/HCPD_analysis/FC_matrices/REST
HOME_D=/fslgroup/fslg_HBN_preproc/compute
PREP_DIR=${HOME_D}/HCPD_analysis/CBIG2016_preproc_FS6_REST

#Submit a job for each sub/sess
#for sub in `cat ${CODE_DIR}/subjids/subjids3.txt`; do
for sub in sub-HCD0977576; do
	SUB=`basename "$sub"`
	for SES in 1 2 3 4 5 6 7 8 9; do
		FILE=$PREP_DIR/${SUB}/${SUB}/surf/lh.${SUB}_bld00${SES}_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz
	        #if file exists
       		if [ -f "$FILE" ]; then
			#grab the run's number of volumes
	                #VOL=`fslinfo $FILE | sed -n 5p | cut -f3`
 		        #if [ ${VOL} -eq 474 ]; then  
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
			#else
		#		echo "no $SES $SUB"
		#	fi
		else
			echo " "
fi
done
done
