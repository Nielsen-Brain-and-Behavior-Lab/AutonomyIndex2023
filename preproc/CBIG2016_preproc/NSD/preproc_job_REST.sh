#!/bin/bash

#SBATCH --time=40:00:00   # walltime
#SBATCH --ntasks=1   # number of processor cores (i.e. tasks)
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=51250M   # memory per CPU core
#SBATCH -J "NSDprep"   # job name


# Set the max number of threads to use for programs using OpenMP. Should be <= ppn. Does nothing if the program doesn't use OpenMP.
export OMP_NUM_THREADS=$SLURM_CPUS_ON_NODE

#Load git and matlab modules
#export LD_PRELOAD=/usr/lib/fastx/latest/virtualgl/usr/lib64/libdlfaker.so:/usr/lib/fastx/latest/virtualgl/usr/lib64/libvglfaker.so

ml git/2.14
ml matlab/r2018b
LD_PRELOAD= matlab &
unset DISPLAY


#Set paths! ADD YOUR NETID: 
HOME=/fslgroup/fslg_spec_networks/compute
code_Dir=${HOME}/code/NSD_analysis/CBIG2016_preproc
CBIG_Dir=/fslgroup/fslg_rdoc/compute/CBIG
out_Dir=/fslgroup/fslg_fun_conn/compute/NSD_analysis/CBIG2016_preproc_FS6_REST
subj=$1
HOME2=/fslgroup/fslg_fun_conn/compute
subj_Dir=${HOME2}/NSD_analysis/NSD_BIDS/sub-${subj}

mkdir -p ${out_Dir}

# 'source' -> YOUR <- config script (with software paths)
source ${code_Dir}/CBIG_preproc_tested_config_funconn.sh

# Run the preprocessing pipeline
${CBIG_Dir}/stable_projects/preprocessing/CBIG_fMRI_Preproc2016/CBIG_preproc_fMRI_preprocess.csh \
	-s sub-${subj} \
	-output_d ${out_Dir}/sub-${subj} \
	-anat_s sub-${subj} \
	-anat_d ${subj_Dir}/ses-nsdanat/anat \
	-fmrinii ${subj_Dir}/sub-${subj}_REST_fmrinii.txt \
	-config ${code_Dir}/example_config.txt 

