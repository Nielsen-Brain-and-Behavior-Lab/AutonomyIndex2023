% Purpose: Calculate the Autonomy Index (AI) following formula in Wang et al. 2014.
% Inputs: An averaged FC matrix (fsaverage6) for each participant.
% Outputs: rh_labels and lh_labels containing AI values for each vertex.
%
% Written by M. Peterson, Nielsen Brain and Behavior Lab, under MIT License 2022

% To run: 
%	 1. Claim computing resources using salloc (ex: `salloc --mem-per-cpu 300G --time 48:00:00 --x11`)
%	 2. Load matlab module: `ml matlab/r2018b`
%	 3. Enter the command `LD_PRELOAD= matlab`


% Set paths
project_dir = '/fslgroup/fslg_csf_autism/compute/HCP_analysis/parc_output_fs6_HCP_ALL/quant_metrics/FC_matrices';
out_dir = '/fslgroup/fslg_csf_autism/compute/HCP_analysis/parc_output_fs6_HCP_ALL/quant_metrics/ai_values';
sublist = [ "SUB" ];

% Loop through each matrix and vertex and calculate AI
for sub = sublist

	%load FC matrix
	FC_file = strcat(sub, '_avg_FC.mat');
	FC_full = fullfile(project_dir, FC_file);
	loaded_matrix = load(FC_full);
    new = loaded_matrix.ind_avg;

	%grab matrix size
	rows = size(new, 1);

	%split matrix in half row-wise into LH and RH (LH is top right corner of matrix as determined in CBIG_ComputeFullSurfaceCorrelation.m)
	lh_matrix = new(1:(rows/2),:); 
	rh_matrix = new((rows/2 + 1):end,:);
    
    %loop through each row of lh_matrix and calculate AI
    for idx = 1:size(lh_matrix,1)
        lh_labels(idx) = ai_left(lh_matrix(idx,:));
        lh_labels = lh_labels';
    end
    
    %loop through each row of rh_matrix and calculate AI
    for idx = 1:size(rh_matrix,1)
        rh_labels(idx) = ai_right(rh_matrix(idx,:));
        rh_labels = rh_labels';
    end
        
    %write output as rh_labels and lh_labels in a .mat file
    if(~exist(out_dir))
        mkdir(out_dir);
    end
    
    save(fullfile(out_dir, strcat(sub, '_FS6_AI.mat')), 'lh_labels','rh_labels');

end


%function for ai on left hemisphere matrix
function [AI_left] = ai_left(x)
		clmns = numel(x);   %fsaverage6 space, so each hemi is same number of vertices
		corrI = sum(x(1:(clmns/2))>= abs(0.25));		 
		totalI = clmns/2; 
		corrC = sum(x((clmns/2 +1):round(end))>= abs(0.25));
		totalC = clmns/2;
		AI_left = (corrI/totalI) - (corrC/totalC);
        fprintf('AI calculated at ...\n', AI_left);
end

%function for ai on right hemisphere matrix
function [AI_right] = ai_right(x)
		clmns = numel(x);   %fsaverage6 space, so each hemi is same number of vertices
		corrI = sum(x((clmns/2 +1):round(end))>= abs(0.25));
        totalI = clmns/2; 
		corrC = sum(x(1:(clmns/2))>= abs(0.25));		
		totalC = clmns/2;
		AI_right = (corrI/totalI) - (corrC/totalC);
        fprintf('AI calculated at ...\n', AI_right);
end
