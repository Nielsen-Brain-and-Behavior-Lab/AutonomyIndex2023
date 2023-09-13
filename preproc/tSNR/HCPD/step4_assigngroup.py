#!/usr/bin/env python
# Purpose: Create a .mat file of vectors with assigned avg tSNR values using MSHBM parc to guide assignment.
# Input: group parcellation .mat file from Kong2019 pipeline and a 17-item list of avg-tSNR values
# Output: 1 .mat file with rh_labels and lh_labels of tSNR values
#
# Note: Users may need to run 'which python' and paste the correct path following the shebang on line one.
# Note: Please use python v.3.6. Ex: `ml python/3.6`
#
# Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License 2023

from pathlib import Path
import time
import os
from os.path import dirname, join as pjoin
import sys
import scipy.io #loads .mat files
import csv
import numpy as np
import pandas as pd

#Read LH and RH SNR .csv file
lh_df = pd.read_csv('HCPD_tSNR_LH_230911.csv')
rh_df = pd.read_csv('HCPD_tSNR_RH_230911.csv')

# Extract the values as a list
lh_values = lh_df['LH_AVG_SNR'].tolist()
rh_values = rh_df['RH_AVG_SNR'].tolist()

# Set network 0 to -100; network 0 = medial wall)
lh_values.insert(0, -100)
rh_values.insert(0, -100)

count=0
for network in range(0,18): #Range must be set to actual number of networks +1
		count=(count+1)
		# set sub name
		sub_name='GROUP'

		# path to parcellation output
		test_dir: str="/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/parc_output_fs6_HCPD_REST/generate_profiles_and_ini_params/group"
		test_name = "343_group_matched.mat" #make sure your file is Hungarian-matched!
		test_sub = pjoin(test_dir, test_name)
		test_file = scipy.io.loadmat(test_sub) #Load first sub .mat file
		test_rh = np.squeeze(test_file['rh_labels']) 
		test_rh_list = test_rh.tolist()			
		test_lh = np.squeeze(test_file['lh_labels'])
		test_lh_list = test_lh.tolist()
		
		if count==1:
			new_lh_list = [lh_values[network] if x == network else x for i, x in enumerate(test_lh_list)]
			new_rh_list = [rh_values[network] if x == network else x for i, x in enumerate(test_rh_list)]
		else:
			new_lh_list = [lh_values[network] if x == network else x for i, x in enumerate(new_lh_list)]
			new_rh_list = [rh_values[network] if x == network else x for i, x in enumerate(new_rh_list)]
	
#Save output to .mat with rh_labels and lh_labels vectors
scipy.io.savemat('HCPD_GROUP_tSNR_230911.mat', {'rh_labels': new_rh_list, 'lh_labels': new_lh_list})


