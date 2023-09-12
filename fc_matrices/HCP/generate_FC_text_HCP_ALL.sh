#!/bin/bash

# Purpose: Generate text files to create individual vertex-to-vertex FC matrices.
# Inputs: Surface-projected timeseries (rsfMRI)
# Outputs: Vertex-to-vertex FC matrices for each individual
# Written by M. Peterson, Nielsen Brain and Behavior Lab under MITT License 2022

##########################
# Specify paths
##########################

HOME=/fslgroup/fslg_spec_networks/compute
code_dir=${HOME}/code/HCP_analysis/FC_matrices
preproc_output=/fslgroup/fslg_autism_networks/compute/HCP_analysis/CBIG2016_preproc_FS6_ALL
out_dir=/fslgroup/grp_parc/compute/HCP_analysis/parc_output_fs6_HCP_ALL/quant_metrics/FC_matrices
mkdir -p $out_dir

#########################################
# Create data lists
#########################################

mkdir -p $out_dir/data_list

for SUB in ${preproc_output}/*/; do
	sub=`basename "$SUB"`
    for sess in {1..4}; do
    if [ "${sess}" -lt 10 ]; then
	    	FILE=$preproc_output/${sub}/${sub}/surf/lh.${sub}_bld00${sess}_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz
	    #if file exists
	    if [ -f "$FILE" ]; then
		    #grab the run's number of volumes
		    #VOL=`fslinfo $FILE | sed -n 5p | cut -f3`
		    #if [ ${VOL} -eq 1196 ]; then  
			# fMRI data
	                lh_fmri="$preproc_output/${sub}/${sub}/surf/lh.${sub}_bld00${sess}_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz"
    			echo $lh_fmri >> $out_dir/data_list/${sub}_sess-${sess}_LH.txt

	    		rh_fmri="$preproc_output/${sub}/${sub}/surf/rh.${sub}_bld00${sess}_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz"
		    	echo $rh_fmri >> $out_dir/data_list/${sub}_sess-${sess}_RH.txt
		    #else
			#echo "no $sess $sub"
		    #fi
           else
		echo ""
	   fi
    else		
	FILE=${preproc_output}/${sub}/${sub}/surf/lh.${sub}_bld0${sess}_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz
	if [ -f "$FILE" ]; then
	    #check if file is rsfMRI scan (volumes =474)
	    #grab the run's number of volumes
	    VOL=`fslinfo $FILE | sed -n 5p | cut -f3`
	    if [ ${VOL} -eq 1196 ]; then  
		# fMRI data
                lh_fmri="$preproc_output/${sub}/${sub}/surf/lh.${sub}_bld0${sess}_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz"
		echo $lh_fmri >> $out_dir/data_list/${sub}_sess-${sess}_LH.txt

    		rh_fmri="$preproc_output/${sub}/${sub}/surf/rh.${sub}_bld0${sess}_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz"
	    	echo $rh_fmri >> $out_dir/data_list/${sub}_sess-${sess}_RH.txt
		    else
			echo "no $sess $sub"
		    fi
		echo "no $sess $sub"
           fi
    fi
    done
done
