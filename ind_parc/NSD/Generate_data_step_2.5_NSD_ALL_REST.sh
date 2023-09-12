#!/bin/bash

# This is Step 2.5, which will generate the profile lists for Step 3

##########################
# Specify output directory
##########################

HOME=/fslgroup/fslg_spec_networks/compute
HOME_D=/fslgroup/fslg_fun_conn/compute
out_dir=${HOME_D}/NSD_analysis/parc_output_fs6_NSD_ALL_REST
preproc_output=${HOME_D}/NSD_analysis/CBIG2016_preproc_FS6_REST
list_dir=${out_dir}/generate_individual_parcellations/profile_list/test_set
profile_dir=${out_dir}/generate_profiles_and_ini_params/profiles
code_dir=${HOME}/code/NSD_analysis/Kong2019_parc_all_REST
sess_dir=${code_dir}/sess_lists
########################################
# Generate profile lists--Step3 Test Set 
########################################

mkdir -p $list_dir $sess_dir

#1. create list of sessions available for each subject
for sub in `cat ${code_dir}/subjids.txt`; do
	for sess in {1..34}; do
		FILE=$out_dir/generate_profiles_and_ini_params/profiles/sub${sub}/sess${sess}/\
lh.sub${sub}_sess${sess}_fsaverage6_roifsaverage3.surf2surf_profile.nii.gz

		#if file exists, add to sub sesslist
        	if [ -f "$FILE" ]; then
		lh_profile="$out_dir/generate_profiles_and_ini_params/profiles/sub${sub}/sess${sess}/\
lh.sub${sub}_sess${sess}_fsaverage6_roifsaverage3.surf2surf_profile.nii.gz"
		echo $lh_profile >> $sess_dir/sub-${sub}_lh.txt
						
		rh_profile="$out_dir/generate_profiles_and_ini_params/profiles/sub${sub}/sess${sess}/\
rh.sub${sub}_sess${sess}_fsaverage6_roifsaverage3.surf2surf_profile.nii.gz"
		echo $rh_profile >> $sess_dir/sub-${sub}_rh.txt

		else
		echo "sess $sess not available for sub $sub"
		fi		
done

		#add filler lines equal to number of missing sessions to the end of the file
		num_lines=`wc --lines < ${sess_dir}/sub-${sub}_lh.txt`
		max_sess=34
		filler=`expr $max_sess - $num_lines`
		
		if [ "$filler" -ne 0 ]; then
		for i in $( seq 1 $filler ); do
		filler_message="$out_dir/generate_profiles_and_ini_params/profiles/sub${sub}/sess${sess}/\
rh.sub${sub}_sess35_fsaverage6_roifsaverage3.surf2surf_profile.nii.gz"
		echo $filler_message >> ${sess_dir}/sub-${sub}_lh.txt
		echo $filler_message >> ${sess_dir}/sub-${sub}_rh.txt 
		done
		fi
done


#2. RH: if # line exists, add to list# otherwise, dummy line
for sub in `cat ${code_dir}/subjids.txt`; do
	filename=${sess_dir}/sub-${sub}_rh.txt
	n=1
	while read line; do
		if [ -z "$line" ]; then
		echo "this is a fake line" >> $list_dir/rh_sess${n}.txt
		else
		echo $line >> $list_dir/rh_sess${n}.txt	
		fi
	n=$((n+1))
	done < $filename
done

#2. LH
for sub in `cat ${code_dir}/subjids.txt`; do
	filename=${sess_dir}/sub-${sub}_lh.txt
	n=1
	while read line; do
		if [ -z "$line" ]; then
		echo "this is a fake line" >> $list_dir/lh_sess${n}.txt
		else		
		echo $line >> $list_dir/lh_sess${n}.txt	
		fi
	n=$((n+1))
	done < $filename
done



echo "Lists successfully generated! Step 2.5 is complete."
