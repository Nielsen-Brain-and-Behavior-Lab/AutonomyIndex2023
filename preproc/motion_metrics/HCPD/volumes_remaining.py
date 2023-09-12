#!/usr/bin/env python
# Purpose: Find remaining volumes (post-censoring) for each session and for each participant overall.
#
# Input: Single-column motion outliers txt file for each bold image. These were generated using the CBIG2016 preproc pipeline.
# Output: Total remaining volumes for each participant's session an deach participant overall. 
#
# Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License (2021)

#load packages
from pathlib import Path
import time
import os
from os.path import dirname, join as pjoin
import sys
import scipy.io #loads .mat files
import csv
import numpy as np
import pandas as pd
import glob

#for each subject, load the FDRMS files for each session
directory = '/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/CBIG2016_preproc_FS6_REST/' #set the directory to your preproc output
sub_count = 0
for sub in os.scandir(directory): #loop through each subject in the preproc dir
	sub_count = sub_count + 1 
	sub_name = str(sub.name) + "/" + str(sub.name) + "/qc"
	subdir = pjoin(directory, sub_name)
	counter = 0
	for sess in glob.iglob(f'{subdir}/*_FDRMS0.2_DVARS50_motion_outliers.txt'): #loop through each bold session
		counter = counter + 1
		sess_df = pd.read_table(sess, header=None, sep='\s+')

		s_sum = sess_df.sum(axis='rows') #take average of column

		#append sess avg to running_list
		if counter==1:
			running_list = []
			running_list.append(s_sum)
		else:
			running_list.append(s_sum)
	
	sub_sum = np.sum(np.array(running_list)) #find the subject's avg FD
	
	#append sub's avg to list
	if sub_count==1:
		sub_list = []
		sub_list.append(str(sub.name))
		sum_list = []
		sum_list.append(str(sub_sum))
	else:
		sub_list.append(str(sub.name))
		sum_list.append(str(sub_sum))


#Save the sub_avg list as .csv
r_name = "RemainingVols_HCPD_230517.csv" #file name

df = pd.DataFrame(list(zip(sub_list, sum_list)), columns = ['ID', 'Sum_Volumes']) #create two-column dataframe with subject name and avg FD

df.to_csv(r_name, index=False, index_label=None) #save dataframe to csv

