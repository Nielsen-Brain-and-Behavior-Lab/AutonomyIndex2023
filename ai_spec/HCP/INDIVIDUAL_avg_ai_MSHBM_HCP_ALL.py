#!/usr/bin/env python
# Purpose: Create a .csv file of network-averaged AI values using MSHBM parc.
# Input: ind_parcellation output from Kong2019 pipeline and ai output
# Output: 1 .csv file with network-averaged AI values
#
# Note: Users may need to run 'which python' and paste the correct path following the shebang on line one.
#
# Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License (2021)

from pathlib import Path
import time
import os
from os.path import dirname, join as pjoin
import sys
import scipy.io #loads .mat files
import csv
import numpy as np
import pandas as pd

# create function to get index positions
def get_index_positions(list_of_elems, element):
	index_pos_list = []
	index_pos = 0
	while True:
		try:
			index_pos = list_of_elems.index(element, index_pos)
			index_pos_list.append(index_pos)
			index_pos += 1
		except ValueError as e:
			break
	return index_pos_list


# Open the text file containing the subject IDs

with open('/fslgroup/fslg_spec_networks/compute/code/HCP_analysis/Kong2019_parc_fs6_ALL/subjids/ids.txt', 'r') as f:
    # Read the lines of the file and store them in a list
    subject_ids = f.read().splitlines()

# Loop through subject IDs to finding avg. AI within network
count=0
sub_count=0
for sub1 in subject_ids:
	sub_count=(sub_count+1)
	for network in range(0,18): #Range must be set to actual number of networks +1
		count=(count+1)
		# set sub name
		sub_name=str(sub1)

		# path to parcellation output
		test_dir: str="/fslgroup/grp_hcp/compute/HCP_analysis/parc_output_fs6_HCP_ALL/generate_individual_parcellations/ind_parcellation/test_set"
		test_name = "Ind_parcellation_MSHBM_sub" + str(sub_count) + "_w200_MRF30_matched.mat"
		test_sub = pjoin(test_dir, test_name)
		test_file = scipy.io.loadmat(test_sub) #Load first sub .mat file
		test_rh = np.squeeze(test_file['rh_labels']) 
		test_rh_list = test_rh.tolist()			
		test_lh = np.squeeze(test_file['lh_labels'])
		test_lh_list = test_lh.tolist()
		
		# path to ai output
		sec_dir: str="/fslgroup/fslg_csf_autism/compute/HCP_analysis/parc_output_fs6_HCP_ALL/quant_metrics/ai_values"
		sec_name = "sub-" + sub_name + "_FS6_AI.mat"
		sec_sub = pjoin(sec_dir, sec_name)
		sec_file = scipy.io.loadmat(sec_sub)
		sec_rh = np.squeeze(sec_file['rh_labels'])
		sec_rh_list = sec_rh.tolist()
		sec_lh = np.squeeze(sec_file['lh_labels'])
		sec_lh_list = sec_lh.tolist()

		#find total vertices for a given network
		#rh
		rh_tot = test_rh_list.count(network) #Total vertices for $network in RH
		#lh
		lh_tot = test_lh_list.count(network) #Total vertices for $network in LH
		
		#find positions of vertices labelled with a given network
		#rh
		index_pos_list = get_index_positions(test_rh_list, network) #Get positions/order for $network 
		rh_pos_list = index_pos_list 
		index_pos_list = None
		#lh
		index_pos_list = get_index_positions(test_lh_list, network) #Get positions/order for $network for sub2
		lh_pos_list = index_pos_list 
		index_pos_list = None

		#create avg ai values
		#avg rh ai
		r_network_ai = [sec_rh_list[i] for i in rh_pos_list] #Make new list of ai values for a given network
		rh_ai_sum = sum(r_network_ai) #Add ai values for RH for a given network
		rh_mean_ai = rh_ai_sum / rh_tot #Take mean ai for RH 
		
		#avg lh ai
		l_network_ai = [sec_lh_list[i] for i in lh_pos_list]
		lh_ai_sum = sum(l_network_ai)
		lh_mean_ai = lh_ai_sum / lh_tot #Take mean ai for LH
		
		#avg total ai
		sum_ai = rh_ai_sum + lh_ai_sum #add rh and lh ai totals
		total_elements = rh_tot + lh_tot #add rh and lh total number of elements
		ai_avg = sum_ai / total_elements #total avg of ai = sum_ai/total_elements		

		#save output to running list
		if count==1:
			runningList = [[sub_name, network, lh_tot, rh_tot, rh_mean_ai, lh_mean_ai, ai_avg]] 
			df = pd.DataFrame(runningList, columns=['SUBJID', 'Network', 'LH_TOT', 'RH_TOT', 'RH_AVG_AI', 'LH_AVG_AI', 'Network_AVG_AI'])
		else:
			runningList = [[sub_name, network, lh_tot, rh_tot, rh_mean_ai, lh_mean_ai, ai_avg]]
			df = df.append(pd.DataFrame(runningList, columns=['SUBJID', 'Network', 'LH_TOT', 'RH_TOT', 'RH_AVG_AI', 'LH_AVG_AI', 'Network_AVG_AI']), ignore_index=True)

#Save the dataframe as .csv
r_name = "MSHBM_LONG_AVG_AI_HCP_ALL_230221.csv" #file name
df.to_csv(r_name, index=False, index_label=None) #save dataframe to csv
