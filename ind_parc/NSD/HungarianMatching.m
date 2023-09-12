%Purpose: Implement Hungarian Matching Algorithm for MSHBM and k-means parcellations 
%parcellations.
%Inputs: MSHBM and k-means parcellations (run directly following
%parcellation BEFORE visualization)
%Outputs: Individual parcellations with matched values (e.g., network 1 is the same across parcellation methods. 
%
%To run: 1. Open Matlab using salloc (ex: `salloc --mem-per-cpu 6G --time 2:00:00 --x11`)
%	 2. source your config file containing the $CBIG_CODE_DIR variable
%	 3. `cd` to the $CBIG_CODE_DIR/stable_projects/brain_parcellation/Kong2019_MSHBM/step3... folder
% 	 4. `cp` this script over to the step3 folder in the CBIG repo
%	 5. Enter the command `ml matlab/r2018b`
%	 6. Enter the command `LD_PRELOAD= matlab`
%	 7. In Matlab: Pull up this script and choose "Run" (green button)
%	
% Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License
% 2022.


%% NSD REST ALL
for sub = 1:8
   
    %project dir
    project_dir_m = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_ALL_REST/generate_individual_parcellations/ind_parcellation/test_set';

    
    %load MSHBM parcellations
    sub_str = num2str(sub);
    sub_filename = strcat('Ind_parcellation_MSHBM_sub',sub_str,'_w200_MRF30.mat');
    input_file = fullfile(project_dir_m,sub_filename);
    
    %load reference
    ref_file = '/fslgroup/fslg_spec_networks/compute/results/fsaverage_surfaces/17Network_Reference_FS6/17Network_Reference_FS6_Labels_220808.mat';
    
    %output file name
    output_file = fullfile(project_dir_m, strcat('Ind_parcellation_MSHBM_sub', sub_str, '_w200_MRF30_matched.mat'));
    
    % Implement CBIG Hungarian Cluster Match Surf Wrapper Script
    CBIG_HungarianClusterMatchSurfWrapper(char(ref_file), char(input_file), char(output_file));
    
end    


 %% NSD REST EVEN
for sub = 1:8
   
    %project dir
    project_dir_m = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_EVEN_REST/generate_individual_parcellations/ind_parcellation/test_set';

    
    %load MSHBM parcellations
    sub_str = num2str(sub);
    sub_filename = strcat('Ind_parcellation_MSHBM_sub',sub_str,'_w200_MRF30.mat');
    input_file = fullfile(project_dir_m,sub_filename);
    
    %load reference
    ref_file = '/fslgroup/fslg_spec_networks/compute/results/fsaverage_surfaces/17Network_Reference_FS6/17Network_Reference_FS6_Labels_220808.mat';
    
    %output file name
    output_file = fullfile(project_dir_m, strcat('Ind_parcellation_MSHBM_sub', sub_str, '_w200_MRF30_matched.mat'));
    
    % Implement CBIG Hungarian Cluster Match Surf Wrapper Script
    CBIG_HungarianClusterMatchSurfWrapper(char(ref_file), char(input_file), char(output_file));
    
end    


%% NSD REST ODD
for sub = 1:8
   
    %project dir
    project_dir_m = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_ODD_REST/generate_individual_parcellations/ind_parcellation/test_set';

    
    %load MSHBM parcellations
    sub_str = num2str(sub);
    sub_filename = strcat('Ind_parcellation_MSHBM_sub',sub_str,'_w200_MRF30.mat');
    input_file = fullfile(project_dir_m,sub_filename);
    
    %load reference
    ref_file = '/fslgroup/fslg_spec_networks/compute/results/fsaverage_surfaces/17Network_Reference_FS6/17Network_Reference_FS6_Labels_220808.mat';
    
    %output file name
    output_file = fullfile(project_dir_m, strcat('Ind_parcellation_MSHBM_sub', sub_str, '_w200_MRF30_matched.mat'));
    
    % Implement CBIG Hungarian Cluster Match Surf Wrapper Script
    CBIG_HungarianClusterMatchSurfWrapper(char(ref_file), char(input_file), char(output_file));
    
end    




%% NSD REST FIRST HALF
for sub = 1:8
   
    %project dir
    project_dir_m = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_FIRST_HALF_REST/generate_individual_parcellations/ind_parcellation/test_set';

    
    %load MSHBM parcellations
    sub_str = num2str(sub);
    sub_filename = strcat('Ind_parcellation_MSHBM_sub',sub_str,'_w200_MRF30.mat');
    input_file = fullfile(project_dir_m,sub_filename);
    
    %load reference
    ref_file = '/fslgroup/fslg_spec_networks/compute/results/fsaverage_surfaces/17Network_Reference_FS6/17Network_Reference_FS6_Labels_220808.mat';
    
    %output file name
    output_file = fullfile(project_dir_m, strcat('Ind_parcellation_MSHBM_sub', sub_str, '_w200_MRF30_matched.mat'));
    
    % Implement CBIG Hungarian Cluster Match Surf Wrapper Script
    CBIG_HungarianClusterMatchSurfWrapper(char(ref_file), char(input_file), char(output_file));
    
end    


%% NSD REST SECOND HALF
for sub = 1:8
   
    %project dir
    project_dir_m = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_SECOND_HALF_REST/generate_individual_parcellations/ind_parcellation/test_set';

    
    %load MSHBM parcellations
    sub_str = num2str(sub);
    sub_filename = strcat('Ind_parcellation_MSHBM_sub',sub_str,'_w200_MRF30.mat');
    input_file = fullfile(project_dir_m,sub_filename);
    
    %load reference
    ref_file = '/fslgroup/fslg_spec_networks/compute/results/fsaverage_surfaces/17Network_Reference_FS6/17Network_Reference_FS6_Labels_220808.mat';
    
    %output file name
    output_file = fullfile(project_dir_m, strcat('Ind_parcellation_MSHBM_sub', sub_str, '_w200_MRF30_matched.mat'));
    
    % Implement CBIG Hungarian Cluster Match Surf Wrapper Script
    CBIG_HungarianClusterMatchSurfWrapper(char(ref_file), char(input_file), char(output_file));
    
end    

%% NSD REST RAND1_1
for sub = 1:8
   
    %project dir
    project_dir_m = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_RAND1_1_REST/generate_individual_parcellations/ind_parcellation/test_set';

    
    %load MSHBM parcellations
    sub_str = num2str(sub);
    sub_filename = strcat('Ind_parcellation_MSHBM_sub',sub_str,'_w200_MRF30.mat');
    input_file = fullfile(project_dir_m,sub_filename);
    
    %load reference
    ref_file = '/fslgroup/fslg_spec_networks/compute/results/fsaverage_surfaces/17Network_Reference_FS6/17Network_Reference_FS6_Labels_220808.mat';
    
    %output file name
    output_file = fullfile(project_dir_m, strcat('Ind_parcellation_MSHBM_sub', sub_str, '_w200_MRF30_matched.mat'));
    
    % Implement CBIG Hungarian Cluster Match Surf Wrapper Script
    CBIG_HungarianClusterMatchSurfWrapper(char(ref_file), char(input_file), char(output_file));
    
end    

%% NSD REST RAND1_2
for sub = 1:8
   
    %project dir
    project_dir_m = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_RAND1_2_REST/generate_individual_parcellations/ind_parcellation/test_set';

    
    %load MSHBM parcellations
    sub_str = num2str(sub);
    sub_filename = strcat('Ind_parcellation_MSHBM_sub',sub_str,'_w200_MRF30.mat');
    input_file = fullfile(project_dir_m,sub_filename);
    
    %load reference
    ref_file = '/fslgroup/fslg_spec_networks/compute/results/fsaverage_surfaces/17Network_Reference_FS6/17Network_Reference_FS6_Labels_220808.mat';
    
    %output file name
    output_file = fullfile(project_dir_m, strcat('Ind_parcellation_MSHBM_sub', sub_str, '_w200_MRF30_matched.mat'));
    
    % Implement CBIG Hungarian Cluster Match Surf Wrapper Script
    CBIG_HungarianClusterMatchSurfWrapper(char(ref_file), char(input_file), char(output_file));
    
end    

%% NSD REST 12
for sub = 1:8
   
    %project dir
    project_dir_m = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_12_REST/generate_individual_parcellations/ind_parcellation/test_set';

    
    %load MSHBM parcellations
    sub_str = num2str(sub);
    sub_filename = strcat('Ind_parcellation_MSHBM_sub',sub_str,'_w200_MRF30.mat');
    input_file = fullfile(project_dir_m,sub_filename);
    
    %load reference
    ref_file = '/fslgroup/fslg_spec_networks/compute/results/fsaverage_surfaces/17Network_Reference_FS6/17Network_Reference_FS6_Labels_220808.mat';
    
    %output file name
    output_file = fullfile(project_dir_m, strcat('Ind_parcellation_MSHBM_sub', sub_str, '_w200_MRF30_matched.mat'));
    
    % Implement CBIG Hungarian Cluster Match Surf Wrapper Script
    CBIG_HungarianClusterMatchSurfWrapper(char(ref_file), char(input_file), char(output_file));
    
end    



