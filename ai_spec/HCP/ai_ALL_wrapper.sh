#!/bin/bash

#Purpose: submit jobs to generate individual full-surface FC matrices

#Set paths and vars
HOME=/fslgroup/fslg_spec_networks/compute
CODE_DIR=${HOME}/code/HCP_analysis/ai_spec/ALL/individual_ai

#Submit a job for each sub/sess
for sub in `cat ${CODE_DIR}/subjids.txt`; do
	SUB=sub-${sub}

			#make new matlab and job scripts for each sub/sess
			CODE_DIR2=${CODE_DIR}/subj_scripts/${SUB}
			mkdir -p ${CODE_DIR2}
		
			#matlab script
			matfile=${CODE_DIR}/ai_ALL_SINGLE.m
			cp ${matfile} ${CODE_DIR2}
		
			sed -i 's|SUB|'"${SUB}"'|g' ${CODE_DIR2}/ai_ALL_SINGLE.m

			#job script
			jobfile=${CODE_DIR}/ai_ALL_job.sh
			cp ${jobfile} ${CODE_DIR2}
	
			sed -i 's|${SUB}|'"${SUB}"'|g' ${CODE_DIR2}/ai_ALL_job.sh

			#submit job 
			sbatch ${CODE_DIR2}/ai_ALL_job.sh
	sleep 1
done
