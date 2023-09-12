% Purpose: Convert lh_labels and rh_labels to gifti shape files for later WB
% visualization
% Inputs: lh_labels and rh_labels from calculate_ai.m and template gifti
% files in appropriate resolution. 
% Outputs: .shape.gii files containing AI values.
%
% Note: Template .shape.gii files can be created using mris_vol2surf in
% combination with bbregister. See the following script: project_surface_FS_AVG_FS6.sh
%
% Written by M. Peterson, Nielsen Brain and Behavior Lab, under MIT License 2022

% To run: 
%	 1. Claim computing resources using salloc (ex: `salloc --mem-per-cpu 100G --time 48:00:00 --x11`)
%	 2. Load matlab module: `ml matlab/r2018b`
%	 3. Enter the command `LD_PRELOAD= matlab`

%% HCP Analysis

% Set paths and variables
out_dir = '/fslgroup/fslg_csf_autism/compute/HCP_analysis/parc_output_fs6_HCP_ALL/quant_metrics/ai_values';
gifti_template_dir = '/fslgroup/fslg_spec_networks/compute/results/fsaverage_surfaces';

%Load text file
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


% Loop through each subject
for subid = 1:length(sublist)
    sub=sublist(subid);

    %load AI data
    infile = strcat('sub-', sub, '_FS6_AI.mat');
    inputfull = fullfile(out_dir, infile);
    if isfile(inputfull)
        load(inputfull)
    
        %grab resolution
        resolution = size(lh_labels,1);
          
        %output filenames
        fname_lh = strcat('sub-', sub, '_FS6_AI_lh.shape.gii');
        fname_rh = strcat('sub-', sub, '_FS6_AI_rh.shape.gii');
        full_lh = fullfile(out_dir, fname_lh);
        full_rh = fullfile(out_dir, fname_rh);
    
        %load in template .shape.gii file 
        g_left = gifti(fullfile(gifti_template_dir, 'FS6_lh.shape.gii'));
        g_right = gifti(fullfile(gifti_template_dir, 'FS6_rh.shape.gii'));
    
        %replace vertex values in templates with AI values
        metric = single(ones(resolution, 1));
        g_left.cdata = metric;
        g_right.cdata = metric;
        g_left.cdata = lh_labels;
        g_right.cdata = rh_labels;
    
        %save output
        save(g_left, char(full_lh));
        save(g_right, char(full_rh));
    else
    end
end
