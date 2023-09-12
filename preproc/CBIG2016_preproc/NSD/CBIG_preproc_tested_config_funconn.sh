#! /bin/sh
# Last successfully run on Jan 12nd, 2018
# Written by CBIG under MIT license: https://github.com/ThomasYeoLab/CBIG/blob/master/LICENSE.md

# DO NOT CHANGE: This clears old freesurfer variables if they previously exist
if [ -n "$FREESURFER_HOME" ]; then
	$FREESURFER_HOME/bin/clear_fs_env.csh 
fi

# PLEASE CHANGE: Please specify location of CBIG repository
export CBIG_CODE_DIR=/fslgroup/fslg_rdoc/compute/CBIG
#Maddy's CBIG DIR
#export CBIG_CODE_DIR=/fslhome/mpeter55/Downloads/CBIG


# PLEASE CHANGE: define locations for these libraries
export FREESURFER_HOME=/fslgroup/fslg_rdoc/compute/new_software/freesurfer
export CBIG_MATLAB_DIR=/apps/matlab/r2018b
export CBIG_SPM_DIR=/fslgroup/fslg_rdoc/compute/new_software/spm12
export CBIG_AFNI_DIR=/fslgroup/fslg_rdoc/software/CBIG-software/abin
export CBIG_ANTS_DIR=/fslgroup/fslg_rdoc/compute/new_software/antsbin/bin
export CBIG_WB_DIR=/fslgroup/fslg_rdoc/compute/new_software/workbench/
#export CBIG_CARET_DIR=/apps/arch/Linux_x86_64/caret/
export CBIG_FSLDIR=/fslgroup/fslg_rdoc/compute/new_software/fsl
export PATH=/fslgroup/fslg_rdoc/compute/new_software/git-annex.linux:$PATH



# DO NOT CHANGE: define locations for unit tests data and replication data
#export CBIG_TESTDATA_DIR=/mnt/eql/yeo1/CBIG_test_data/unit_tests
#export CBIG_REPDATA_DIR=NA
#export CBIG_preproc_REP_GSP_DIR=/mnt/eql/yeo3/data/GSP2016

# DO NOT CHANGE: define scheduler location
#export CBIG_SCHEDULER_DIR=/apps/sysapps/TORQUE/bin

# DO NOT CHANGE: set up your environment with the configurations above
SETUP_PATH=${CBIG_CODE_DIR}/setup/CBIG_generic_setup.sh
source $SETUP_PATH

# DO NOT CHANGE: set up temporary directory for MRIread from FS6.0
export TMPDIR=/tmpstore

# Do NOT CHANGE: set up MATLABPATH so that MATLAB can find startup.m in our repo 
export MATLABPATH=/fslgroup/fslg_rdoc/compute/CBIG-master-updated/setup

# specified the default Python environment.
# Please UNCOMMENT if you follow CBIG's set up for Python environments.
# We use Python version 3.5 as default.
# Please see $CBIG_CODE_DIR/setup/python_env_setup/README.md for more details.
# source activate CBIG_py3
