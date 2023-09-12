%Step 1 of CBIG Kong2019 Brain Parc Pipeline
%
%To run: 1. Open Matlab using salloc (ex: `salloc --mem-per-cpu 200G --time 2:00:00 --x11`)
%	 2. source your config file containing the $CBIG_CODE_DIR variable
%	 	-It might be helpful to `cd` to the $CBIG_CODE_DIR/stable_projects/brain_parcellation/Kong2019_MSHBM/step1... folder
% 	 3. Enter the command `ml matlab/r2018b`
%	 4. Enter the command `LD_PRELOAD= matlab`
%	 5. In Matlab: pull up this script and choose "Run"
%
%FYI, the data_list/fMRI_list lists should be formatted sub01_sess1, etc (such that there are no dashes or spaces between "sub" and "01" or "sess" and "1"
%
%Previously, recon-all and the CBIG preproc pipeline were run on these subjects. 
%Additionally, you must have the folder structure and text files with paths to your preproc output set up 
%See the CBIG example script CBIG_MSHBM_create_example_input_data.sh for details on formatting.
%The script "Create_parc_data.sh" has taken care of this.
%
%For questions, contact M. Peterson, Nielsen Brain and Behavior Lab


%Part 1: Generate Profiles for all Subjs 
sublist1 = ["01" "02" "03" "04" "05" "06" "07" "08"];
seslist = [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34];

project_dir = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_ALL_REST/generate_profiles_and_ini_params'
for sub = sublist1
 for sess = seslist
    try  CBIG_MSHBM_generate_profiles('fsaverage3','fsaverage6',project_dir,num2str(sub),num2str(sess),'0');
    continue
     end
end 
end



%Part 2: Create group avg profile
%project_dir = '/fslgroup/fslg_spec_networks/compute/results/MSC_analysis/parc_output_fs6_MSC_ALL/generate_profiles_and_ini_params';
%num_sub = '10';
%num_sess = '10'; %max number of sessions
%CBIG_MSHBM_avg_profiles('fsaverage3','fsaverage6',project_dir,num_sub,num_sess);



%Part 3: Create group.mat with group parc - 17 NETWORKS
%project_dir = '/fslgroup/fslg_spec_networks/compute/results/MSC_analysis/parc_output_fs6_MSC_ALL/generate_profiles_and_ini_params';
%num_clusters = '17';
%num_initialization = '1000';
%CBIG_MSHBM_generate_ini_params('fsaverage3','fsaverage6',num_clusters,num_initialization, project_dir)


%Troubleshooting help
%
%Error: fscanf cannot open... 
%	In this case, your paths somewhere are incorrect. Doublecheck your paths in the generate_profiles_and_ini_params/data_list/fMRI. Also, your project_dir could be incorrect
%   Also, the format of the data_list/fMRI_list text files names may be
%   incorrect
%
%Error: CBIG_MSHBM_generate_ini_params function not found (or something like that)
%	You need to be in the step1 folder to run this script. If you are in a different directory, you will encounter this error.It may help to copy this script over to the script1 directory and then open matlab...
%
%
%
