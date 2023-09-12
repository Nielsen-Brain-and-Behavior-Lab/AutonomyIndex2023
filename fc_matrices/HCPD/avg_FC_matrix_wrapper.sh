#!/bin/bash

#Purpose: submit jobs to generate individual full-surface FC matrices

#Set paths and vars
HOME=/fslgroup/fslg_spec_networks/compute
CODE_DIR=${HOME}/code/HCPD_analysis/FC_matrices/REST

#Submit a job for each sub/sess
#for sub in `cat ${CODE_DIR}/subjids/subjids3.txt`; do
for sub in sub-HCD0977576; do
	SUB=`basename "$sub"`

			#make new matlab and job scripts for each sub/sess
			CODE_DIR2=${CODE_DIR}/subj_scripts/${SUB}
			mkdir -p ${CODE_DIR2}
		
			#matlab script
			matfile=${CODE_DIR}/avg_FC_matrix_SINGLE.m
			cp ${matfile} ${CODE_DIR2}
		
			sed -i 's|SUB|'"${SUB}"'|g' ${CODE_DIR2}/avg_FC_matrix_SINGLE.m

			#job script
			jobfile=${CODE_DIR}/avg_FC_matrix_job.sh
			cp ${jobfile} ${CODE_DIR2}
	
			sed -i 's|${SUB}|'"${SUB}"'|g' ${CODE_DIR2}/avg_FC_matrix_job.sh

			#submit job 
			sbatch ${CODE_DIR2}/avg_FC_matrix_job.sh
	sleep 1
done
