% Wrapper script to turn parcellation files into FreeSurfer annotation
% files. Then you can calculate surface area of parcellations!
%
% Assumes ind_parcellation output from Kong2019 CBIG pipeline.
% Written by M. Peterson, Nielsen Brain and Behavior Lab

%To run: 
%	 1. Claim computing resources using salloc (ex: `salloc --mem-per-cpu 6G --time 2:00:00 --x11`)
%    2. Source your CBIG config file to set up CBIG environment.	 
%    3. Load matlab module: `ml matlab/r2018b`
%	 4. Enter the command `LD_PRELOAD= matlab`


%% HCP_2ALL
project_dir = '/fslgroup/grp_hcp/compute/HCP_analysis/parc_output_fs6_HCP_ALL/generate_individual_parcellations/ind_parcellation/test_set';
out_dir = '/fslgroup/grp_hcp/compute/HCP_analysis/parc_output_fs6_HCP_ALL/quant_metrics/MSHBM_vis';

if(~exist(out_dir))
        mkdir(out_dir);
end

for sub = 1:232
    sub_str = num2str(sub);
    sub_filename = strcat('Ind_parcellation_MSHBM_sub',sub_str,'_w200_MRF30_matched.mat');
    file = fullfile(project_dir,sub_filename);
    lh_name = strcat('sub-',sub_str,'_lh.annot');
    rh_name = strcat('sub-',sub_str,'_rh.annot');
    combo = strcat('sub-',sub_str);
    lh_output_file = fullfile(out_dir,lh_name);
    rh_output_file = fullfile(out_dir,rh_name);
    CBIG_SaveParcellationToFreesurferAnnotation(file, lh_output_file, rh_output_file);
end

