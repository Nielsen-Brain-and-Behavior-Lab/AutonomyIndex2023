%Purpose: Implement CBIG_ComputeFullSurfaceCorrelation function
%Inputs: Text lists for LH and RH data
%Ouputs: Full surface correlation matrices
%
%Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License 2022

%Arg structure:  CBIG_ComputeFullSurfaceCorrelation(output_file, varargin_text1, varargin_text2, pval)
%Instructions: source the CBIG config file, request an salloc job, load matlab/r2018b module, in Matlab run this script

%1c. Single sub and sess trial run  
subout='/fslgroup/grp_nsd/compute/HCPD_analysis/parc_output_fs6_HCPD_REST/quant_metrics/FC_matrices/SUB_sess-SES_fullcorr.mat';
LH_text='/fslgroup/grp_nsd/compute/HCPD_analysis/parc_output_fs6_HCPD_REST/quant_metrics/FC_matrices/data_list/SUB_sess-SES_LH.txt';
RH_text='/fslgroup/grp_nsd/compute/HCPD_analysis/parc_output_fs6_HCPD_REST/quant_metrics/FC_matrices/data_list/SUB_sess-SES_RH.txt';
CBIG_ComputeFullSurfaceCorrelation(subout, LH_text, RH_text, '0')
