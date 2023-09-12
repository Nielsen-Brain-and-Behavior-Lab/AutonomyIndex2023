#!/bin/bash

#Purpose: Determine which subjects have which number of runs available and create lists accordingly for MSHBM Step3.
#Inputs: Generate_data_step_2.5 sess_lists output.
#Outputs: Text lists with corresponding subjects for each number of sessions. 

#Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License 2022.


#Set paths
HOME=/fslgroup/fslg_spec_networks/compute
CODE_DIR=${HOME}/code/HCP_analysis/Kong2019_parc_fs6_ALL
SESS_DIR=${CODE_DIR}/sess_lists_fake
sess_dir=$SESS_DIR
NUM_SESS=${CODE_DIR}/sess_numbers
mkdir -p $SESS_DIR
mkdir -p $NUM_SESS

HOME_D=/fslgroup/grp_hcp/compute
out_dir=${HOME_D}/HCP_analysis/parc_output_fs6_HCP_ALL
list_dir=${out_dir}/generate_individual_parcellations/profile_list/test_set
profile_dir=${out_dir}/generate_profiles_and_ini_params/profiles
code_dir=${HOME}/code/HCP_analysis/Kong2019_parc_fs6_ALL



#create fake sess_list that only includes files that exist
for sub in `cat ${code_dir}/subjids/ids.txt`; do
	for sess in 1 2 3 4; do
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
done




#Grab number of available sessions for each individual
count=0
#Loop through each subject
for sub in `cat ${CODE_DIR}/subjids/ids.txt`; do	
	count=$((count+1))
	#Determine number of sessions for each subj (=length of sess list)
	num_sess=$( cat ${SESS_DIR}/sub-${sub}_lh.txt | wc -l )		

	#Add subject to appropriate list depending on # of sessions
	if [ "$num_sess" -eq 1 ]; then
		echo ${count} >> ${NUM_SESS}/1_sess.txt
	elif [ "$num_sess" -eq 2 ]; then
		echo ${count} >> ${NUM_SESS}/2_sess.txt
	elif [ "$num_sess" -eq 3 ]; then
		echo ${count} >> ${NUM_SESS}/3_sess.txt
	elif [ "$num_sess" -eq 4 ]; then
		echo ${count} >> ${NUM_SESS}/4_sess.txt
	elif [ "$num_sess" -eq 5 ]; then
		echo ${count} >> ${NUM_SESS}/5_sess.txt
	elif [ "$num_sess" -eq 6 ]; then
		echo ${count} >> ${NUM_SESS}/6_sess.txt
	elif [ "$num_sess" -eq 7 ]; then
		echo ${count} >> ${NUM_SESS}/7_sess.txt
	elif [ "$num_sess" -eq 8 ]; then
		echo ${count} >> ${NUM_SESS}/8_sess.txt
	elif [ "$num_sess" -eq 9 ]; then
		echo ${count} >> ${NUM_SESS}/9_sess.txt
	else
		echo ${sub} has 0 sess
	fi

done
