% Purpose: Convert lh_labels and rh_labels to gifti shape files for later WB
% visualization
% Inputs: lh_labels and rh_labels and template gifti
% files in appropriate resolution. (fsaverage6 optimal) 
% Outputs: .shape.gii files containing input label values.
%
% Note: Template .shape.gii files can be created using mris_vol2surf in
% combination with bbregister. See the following script: project_surface_FS_AVG_FS6.sh
%
% Written by M. Peterson, Nielsen Brain and Behavior Lab, under MIT License 2022

% To run: 
%    1. Source the CBIG config script to load GIFTI software
%	 2. Load matlab module: `ml matlab/r2018b`

% Set paths and variables
out_dir = '/fslgroup/fslg_spec_networks/compute/code/HCPD_analysis/tSNR/ALL';
gifti_template_dir = '/fslgroup/fslg_spec_networks/compute/results/fsaverage_surfaces';

% Loop through each subject
    sub='HCPD_GROUP'

    %load input data
    infile = strcat(sub, '_tSNR_230911.mat');
    inputfull = fullfile(out_dir, infile);
    if isfile(inputfull)
        load(inputfull)
    
        lh_labels = lh_labels';
        rh_labels = rh_labels';
        %grab resolution
        resolution = size(lh_labels,1);
          
        %output filenames
        fname_lh = strcat('sub-', sub, '_FS6_tSNR_lh.shape.gii');
        fname_rh = strcat('sub-', sub, '_FS6_tSNR_rh.shape.gii');
        full_lh = fullfile(out_dir, fname_lh);
        full_rh = fullfile(out_dir, fname_rh);
    
        %load in template .shape.gii file 
        g_left = gifti(fullfile(gifti_template_dir, 'FS6_lh.shape.gii'));
        g_right = gifti(fullfile(gifti_template_dir, 'FS6_rh.shape.gii'));
    
        %replace vertex values in templates with values
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
