%Step 3 of CBIG Kong2019 Brain Parc Pipeline
%
%To run: 1. Open Matlab using salloc (ex: `salloc --mem-per-cpu 6G --time 2:00:00 --x11`)
%	 2. source your config file containing the $CBIG_CODE_DIR variable
%	 3. `cd` to the $CBIG_CODE_DIR/stable_projects/brain_parcellation/Kong2019_MSHBM/step3... folder
% 	 4. `cp` this script over to the step3 folder in the CBIG repo
%	 5. Enter the command `ml matlab/r2018b`
%	 6. Enter the command `LD_PRELOAD= matlab`
%	 7. In Matlab: Pull up this script and choose "Run" (green button)
%	
%
%Previously, recon-all and the CBIG preproc pipeline were run on these subjects. 
%Additionally, you must have the folder structure and text files with paths to your preproc output set up 
%See the CBIG example script CBIG_MSHBM_create_example_input_data.sh for details on formatting.
%The script "Create_parc_data.sh" has taken care of this.
%Steps 1 and 2 have also been ran previous to this.
%
%
%For questions, contact M. Peterson, Nielsen Brain and Behavior Lab

%Note: Using the GSP Final_Params (FS6 space) from Ruby and CBIG. Per
%supplementary material for Kong2019 paper, the optimal parameters for the GSP dataset are c=30
%and alpha=200 (see page 10).


%% Part 1: Generate individual parcellation for each subject


%% Subs with 4 runs
%Load text file
    filename = '/nobackup/scratch/grp/fslg_spec_networks/code/HCP_analysis/Kong2019_parc_fs6_ALL/sess_numbers/4_sess.txt';
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

project_dir = '/fslgroup/grp_hcp/compute/HCP_analysis/parc_output_fs6_HCP_ALL/generate_individual_parcellations';
for subid = 1:length(sublist)
        subject=sublist(subid);
       CBIG_MSHBM_generate_individual_parcellation(project_dir,'fsaverage6','4','17',subject,'200','30');
end
