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


%% REST ALL
project_dir = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_ALL_REST/generate_individual_parcellations/ind_parcellation/test_set';
out_dir = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_ALL_REST/quant_metrics/MSHBM_vis';

if(~exist(out_dir))
        mkdir(out_dir);
end

for sub = 1:8
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

%% REST EVEN
project_dir = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_EVEN_REST/generate_individual_parcellations/ind_parcellation/test_set';
out_dir = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_EVEN_REST/quant_metrics/MSHBM_vis';

if(~exist(out_dir))
        mkdir(out_dir);
end

for sub = 1:8
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


%% REST ODD
project_dir = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_ODD_REST/generate_individual_parcellations/ind_parcellation/test_set';
out_dir = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_ODD_REST/quant_metrics/MSHBM_vis';

if(~exist(out_dir))
        mkdir(out_dir);
end

for sub = 1:8
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

%% REST FIRST HALF
project_dir = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_FIRST_HALF_REST/generate_individual_parcellations/ind_parcellation/test_set';
out_dir = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_FIRST_HALF_REST/quant_metrics/MSHBM_vis';

if(~exist(out_dir))
        mkdir(out_dir);
end

for sub = 1:8
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

%% REST SECOND HALF
project_dir = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_SECOND_HALF_REST/generate_individual_parcellations/ind_parcellation/test_set';
out_dir = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_SECOND_HALF_REST/quant_metrics/MSHBM_vis';

if(~exist(out_dir))
        mkdir(out_dir);
end

for sub = 1:8
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

%% REST RAND1_1
project_dir = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_RAND1_1_REST/generate_individual_parcellations/ind_parcellation/test_set';
out_dir = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_RAND1_1_REST/quant_metrics/MSHBM_vis';

if(~exist(out_dir))
        mkdir(out_dir);
end

for sub = 1:8
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

%% REST RAND1_2
project_dir = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_RAND1_2_REST/generate_individual_parcellations/ind_parcellation/test_set';
out_dir = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_RAND1_2_REST/quant_metrics/MSHBM_vis';

if(~exist(out_dir))
        mkdir(out_dir);
end

for sub = 1:8
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

%% REST 12
project_dir = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_12_REST/generate_individual_parcellations/ind_parcellation/test_set';
out_dir = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_12_REST/quant_metrics/MSHBM_vis';

if(~exist(out_dir))
        mkdir(out_dir);
end

for sub = 1:8
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