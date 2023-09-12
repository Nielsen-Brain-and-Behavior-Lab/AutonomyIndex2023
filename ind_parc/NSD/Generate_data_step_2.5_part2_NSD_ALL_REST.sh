#!/bin/bash

#Purpose: Determine which subjects have which number of runs available and create lists accordingly for MSHBM Step3.
#Inputs: Generate_data_step_2.5 sess_lists output.
#Outputs: Text lists with corresponding subjects for each number of sessions. 

#Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License 2022.


#Set paths
HOME=/fslgroup/fslg_spec_networks/compute
CODE_DIR=${HOME}/code/NSD_analysis/Kong2019_parc_all_REST
SESS_DIR=${CODE_DIR}/sess_lists_fake
sess_dir=$SESS_DIR
NUM_SESS=${CODE_DIR}/sess_numbers
mkdir -p $SESS_DIR
mkdir -p $NUM_SESS

HOME_D=/fslgroup/fslg_fun_conn/compute
out_dir=${HOME_D}/NSD_analysis/parc_output_fs6_NSD_ALL_REST
preproc_output=${HOME_D}/NSD_analysis/CBIG2016_preproc_FS6_REST
list_dir=${out_dir}/generate_individual_parcellations/profile_list/test_set
profile_dir=${out_dir}/generate_profiles_and_ini_params/profiles
code_dir=${HOME}/code/NSD_analysis/Kong2019_parc_all_REST



#create fake sess_list that only includes files that exist
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
done




#Grab number of available sessions for each individual
count=0
#Loop through each subject
for sub in `cat ${CODE_DIR}/subjids.txt`; do	
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
	elif [ "$num_sess" -eq 10 ]; then
		echo ${count} >> ${NUM_SESS}/10_sess.txt
	elif [ "$num_sess" -eq 11 ]; then
		echo ${count} >> ${NUM_SESS}/11_sess.txt
	elif [ "$num_sess" -eq 12 ]; then
		echo ${count} >> ${NUM_SESS}/12_sess.txt
	elif [ "$num_sess" -eq 13 ]; then
		echo ${count} >> ${NUM_SESS}/13_sess.txt
	elif [ "$num_sess" -eq 14 ]; then
		echo ${count} >> ${NUM_SESS}/14_sess.txt
	elif [ "$num_sess" -eq 15 ]; then
		echo ${count} >> ${NUM_SESS}/15_sess.txt
	elif [ "$num_sess" -eq 16 ]; then
		echo ${count} >> ${NUM_SESS}/16_sess.txt
	elif [ "$num_sess" -eq 17 ]; then
		echo ${count} >> ${NUM_SESS}/17_sess.txt
	elif [ "$num_sess" -eq 18 ]; then
		echo ${count} >> ${NUM_SESS}/18_sess.txt
	elif [ "$num_sess" -eq 19 ]; then
		echo ${count} >> ${NUM_SESS}/19_sess.txt
	elif [ "$num_sess" -eq 20 ]; then
		echo ${count} >> ${NUM_SESS}/20_sess.txt
	elif [ "$num_sess" -eq 21 ]; then
		echo ${count} >> ${NUM_SESS}/21_sess.txt
	elif [ "$num_sess" -eq 22 ]; then
		echo ${count} >> ${NUM_SESS}/22_sess.txt
	elif [ "$num_sess" -eq 23 ]; then
		echo ${count} >> ${NUM_SESS}/23_sess.txt
	elif [ "$num_sess" -eq 24 ]; then
		echo ${count} >> ${NUM_SESS}/24_sess.txt
	elif [ "$num_sess" -eq 25 ]; then
		echo ${count} >> ${NUM_SESS}/25_sess.txt
	elif [ "$num_sess" -eq 26 ]; then
		echo ${count} >> ${NUM_SESS}/26_sess.txt
	elif [ "$num_sess" -eq 27 ]; then
		echo ${count} >> ${NUM_SESS}/27_sess.txt 
	elif [ "$num_sess" -eq 28 ]; then
		echo ${count} >> ${NUM_SESS}/28_sess.txt
	elif [ "$num_sess" -eq 29 ]; then
		echo ${count} >> ${NUM_SESS}/29_sess.txt
	elif [ "$num_sess" -eq 30 ]; then
		echo ${count} >> ${NUM_SESS}/30_sess.txt
	elif [ "$num_sess" -eq 31 ]; then
		echo ${count} >> ${NUM_SESS}/31_sess.txt
	elif [ "$num_sess" -eq 32 ]; then
		echo ${count} >> ${NUM_SESS}/32_sess.txt
	elif [ "$num_sess" -eq 33 ]; then
		echo ${count} >> ${NUM_SESS}/33_sess.txt
	elif [ "$num_sess" -eq 34 ]; then
		echo ${count} >> ${NUM_SESS}/34_sess.txt
	else
		echo ${sub} has 0 sess
	fi

done
