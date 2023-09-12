#!/bin/bash

#The purpose of this script is to check if the preproc completed
#for every subject. If successful, there will be 6 folders
#for each subject in the resulting count.txt file.

genDir=/fslgroup/fslg_spec_networks/compute
outDir=/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/CBIG2016_preproc_FS6_REST
countDir=${genDir}/code/HCPD_analysis/CBIG2016_preproc

for i in `ls ${outDir}`; do
	if [ "$(find ${outDir}/${i}/${i}/surf -name '*fs6_sm6_fs6.nii.gz' | wc -l)" -gt 0 ]; then
		echo ${i} >> ${countDir}/preproc_completed_ids1.txt
	else
		#mv ${outDir}/${i} ${newDir}/${i}
		echo ${i} >> ${countDir}/preproc_failed_ids1.txt
	fi
done
