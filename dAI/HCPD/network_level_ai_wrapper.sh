#!/bin/bash

#Purpose: submit jobs to generate individual full-surface FC matrices

#Set paths and vars
HOME=/fslgroup/fslg_spec_networks/compute
CODE_DIR=${HOME}/code/HCPD_analysis/ai_spec/REST/network_level_ai

#Submit a job for each sub/sess
count=0
for sub in `cat ${CODE_DIR}/subjids.txt`; do
	SUB=sub-${sub}
	count=$((count+1))
			#make new matlab and job scripts for each sub/sess
			CODE_DIR2=${CODE_DIR}/subj_scripts/${SUB}
			mkdir -p ${CODE_DIR2}
		
			#matlab script
			matfile=${CODE_DIR}/network_level_ai.m
			cp ${matfile} ${CODE_DIR2}
		
			sed -i 's|SUB|'"${SUB}"'|g' ${CODE_DIR2}/network_level_ai.m
			sed -i 's|COUNT|'"${count}"'|g' ${CODE_DIR2}/network_level_ai.m

			#job script
			jobfile=${CODE_DIR}/network_level_ai_job.sh
			cp ${jobfile} ${CODE_DIR2}
	
			sed -i 's|${SUB}|'"${SUB}"'|g' ${CODE_DIR2}/network_level_ai_job.sh

			#submit job 
			sbatch ${CODE_DIR2}/network_level_ai_job.sh
	sleep 1
done
