#!/bin/bash


# This is Step 0: Generate Input Data and setup file folder structures.
# Run this before CBIG Parc Step 1.
# Additionally, the preproc data was formatted sub-0#. Adjust the script as necessary to fit your naming conventions.


##########################
# Specify output directory
##########################

HOME=/fslgroup/fslg_spec_networks/compute
code_dir=${HOME}/code/NSD_analysis/Kong2019_parc_all_REST
HOME_D=/fslgroup/fslg_fun_conn/compute
out_dir=${HOME_D}/NSD_analysis/parc_output_fs6_NSD_ALL_REST
preproc_output=${HOME_D}/NSD_analysis/CBIG2016_preproc_FS6_REST
mkdir -p $out_dir/estimate_group_priors
mkdir -p $out_dir/generate_individual_parcellations

# Add the $CBIG_CODE_DIR to your environment
source ${code_dir}/CBIG_preproc_tested_config_funconn.sh

#########################################
# Create data lists to generate profiles
#########################################

mkdir -p $out_dir/generate_profiles_and_ini_params/data_list/fMRI_list
mkdir -p $out_dir/generate_profiles_and_ini_params/data_list/censor_list

for sub in `cat ${code_dir}/subjids.txt`; do
    for sess in {1..34}; do
	if [ $sess -lt 10 ]; then
	# fMRI data
		lh_fmri="$preproc_output/\
sub-${sub}/sub-${sub}/surf/\
lh.sub-${sub}\
_bld00${sess}_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz"
		
		echo $lh_fmri >> $out_dir/generate_profiles_and_ini_params/data_list/fMRI_list/lh_sub${sub}_sess${sess}.txt
		
		rh_fmri="$preproc_output/\
sub-${sub}/sub-${sub}/surf/\
rh.sub-${sub}\
_bld00${sess}_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz"
		
		echo $rh_fmri >> $out_dir/generate_profiles_and_ini_params/data_list/fMRI_list/rh_sub${sub}_sess${sess}.txt
		
		# censor list
		censor_file="${preproc_output}/sub-${sub}/sub-${sub}/qc/sub-${sub}\
_bld00${sess}_FDRMS0.2_DVARS50_motion_outliers.txt"
		
		echo $censor_file >> $out_dir/generate_profiles_and_ini_params/data_list/censor_list/sub${sub}.txt
		
	else

	# fMRI data
		lh_fmri="$preproc_output/\
sub-${sub}/sub-${sub}/surf/\
lh.sub-${sub}\
_bld0${sess}_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz"
		
		echo $lh_fmri >> $out_dir/generate_profiles_and_ini_params/data_list/fMRI_list/lh_sub${sub}_sess${sess}.txt
		
		rh_fmri="$preproc_output/\
sub-${sub}/sub-${sub}/surf/\
rh.sub-${sub}\
_bld0${sess}_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz"
		
		echo $rh_fmri >> $out_dir/generate_profiles_and_ini_params/data_list/fMRI_list/rh_sub${sub}_sess${sess}.txt
		
		# censor list
		censor_file="${preproc_output}/sub-${sub}/sub-${sub}/qc/sub-${sub}\
_bld0${sess}_FDRMS0.2_DVARS50_motion_outliers.txt"
		
		echo $censor_file >> $out_dir/generate_profiles_and_ini_params/data_list/censor_list/sub${sub}.txt

	fi
	done
done


echo "Step 0 was successful!"

# You are now ready to run Step 1!
