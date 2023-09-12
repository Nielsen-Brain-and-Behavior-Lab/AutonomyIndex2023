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


%% Part 1: Generate Profiles for all Subjs 
%Read in the subjids text file
    filename = '/nobackup/scratch/grp/fslg_spec_networks/code/HCP_analysis/Kong2019_parc_fs6_ALL/subjids/ids.txt';
    delimiter = {''};
    % Format for each line of text:
    %   column1: text (%s)
    % For more information, see the TEXTSCAN documentation.
    formatSpec = '%s%[^\n\r]';
    % Open the text file.
    fileID = fopen(filename,'r');
    % Read columns of data according to the format.
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'TextType', 'string',  'ReturnOnError', false);
    % Close the text file.
    fclose(fileID);
    sublist = [dataArray{1:end-1}];
    clearvars filename delimiter formatSpec fileID dataArray ans;

project_dir = '/fslgroup/grp_hcp/compute/HCP_analysis/parc_output_fs6_HCP_ALL/generate_profiles_and_ini_params';
seslist = [1 2 3 4];
for subid = 1:length(sublist)
    sub=sublist(subid);
    for sess = seslist
        CBIG_MSHBM_generate_profiles('fsaverage3','fsaverage6',char(project_dir),num2str(sub),num2str(sess),'0');
    end 
end


%% Part 2: Create group profile- ALL
%Edit CBIG_MSHBM_avg_profiles to include SUBJID list and any pertinent
%prefixes (ex: "MSC")

project_dir = '/fslgroup/grp_hcp/compute/HCP_analysis/parc_output_fs6_HCP_ALL/generate_profiles_and_ini_params'
num_sub = '232';  %point to correct subjids.txt file
num_sess = '4'; %max number of sessions
CBIG_MSHBM_avg_profiles('fsaverage3','fsaverage6',project_dir,num_sub,num_sess);


%Part 3: Create group.mat with group parc
project_dir = '/fslgroup/grp_hcp/compute/HCP_analysis/parc_output_fs6_HCP_ALL/generate_profiles_and_ini_params'
num_clusters = '17';
num_initialization = '1000';
CBIG_MSHBM_generate_ini_params('fsaverage3','fsaverage6',num_clusters,num_initialization, project_dir)



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
