% Purpose: Calculate network-level autonomy index (AI)
% Inputs: An averaged FC matrix (fsaverage6) for each participant and their
% individual parcellation (Kong2019 MSHBM)
% Outputs: rh_labels and lh_labels (1 .mat file for each of 17 networks)
%
% Written by M. Peterson, Nielsen Brain and Behavior Lab, under MIT License
% 2023

% To run: 
%	 1. Claim computing resources using salloc (ex: `salloc --mem-per-cpu 300G --time 48:00:00 --x11`)
%	 2. Load matlab module: `ml matlab/r2018b`
%	 3. Enter the command `LD_PRELOAD= matlab`


% Set paths
FC_dir = '/fslgroup/grp_nsd/compute/HCP_analysis/parc_output_fs6_HCP_ALL/quant_metrics/FC_matrices';
PARC_dir = '/fslgroup/grp_hcp/compute/HCP_analysis/parc_output_fs6_HCP_ALL/generate_individual_parcellations/ind_parcellation/test_set';
out_dir = '/fslgroup/grp_proc/compute/HCP_analysis/parc_output_fs6_HCP_ALL/quant_metrics/network_level_ai';
%sublist = [ "SUB" ];
%count = [ "COUNT" ]; 
sublist = [ "sub-100307" ];
count = [ "1" ];
% Create out_dir if it doesn't already exist
if(~exist(out_dir))
   mkdir(out_dir);
end

% Loop through each matrix and vertex and calculate AI
for sub = sublist

	%load FC matrix
	FC_file = strcat(sub, '_avg_FC.mat');
	FC_full = fullfile(FC_dir, FC_file);
	loaded_matrix = load(FC_full);
    new = loaded_matrix.ind_avg;

    %load individual parc (Kong2019 MSHBM)
    PARC_file = strcat('Ind_parcellation_MSHBM_sub', count, '_w200_MRF30_matched.mat');
    PARC_full = fullfile(PARC_dir, PARC_file);
    parc = load(PARC_full);
        
	%grab FC matrix size
	rows = size(new, 1);

	%split matrix in half row-wise into LH and RH (LH is top right corner of matrix as determined in CBIG_ComputeFullSurfaceCorrelation.m)
	lh_matrix = new(1:(rows/2),:); 
	rh_matrix = new((rows/2 + 1):end,:);
    
    %loop through each network
    for network = 1:17
        %loop through each row of lh_matrix to calculate network AI
        for idx = 1:size(lh_matrix,1)
                lh_labels(idx) = ai_left(lh_matrix(idx,:), parc, network);
                lh_labels = lh_labels';
        end

        %loop through each row of rh_matrix to calculate network AI
        for idx = 1:size(rh_matrix,1)
            rh_labels(idx) = ai_right(rh_matrix(idx,:), parc, network);
            rh_labels = rh_labels';
        end

        %write output as rh_labels and lh_labels in a .mat file
        save(fullfile(out_dir, strcat(sub, '_NETWORK-', num2str(network), '_AI_FS6.mat')), 'lh_labels','rh_labels');
    end    
end


%Function to identify frequent network contribution for each vertex: LH
function [AI_left] = ai_left(x, labels, network)
    clmns = numel(x);   % fsaverage6 space, so each hemi is same number of vertices
	indices1 = find(x(1:(clmns/2))>= abs(0.25)); % Ipsilateral: Find indices of columns with correlation >= 0.25 for part 1  
	indices2 = find(x((clmns/2 +1):round(end))>= abs(0.25)); % Contralateral: Find indices of columns with correlation >= 0.25 for part 2
	labels1_filtered = labels.lh_labels(indices1); % Filtered labels for part 1
	labels2_filtered = labels.rh_labels(indices2); % Filtered labels for part 2
	
    %find number of highly correlated vertices with labels of $network
    labels1_N_idx_count = size(indices1(labels1_filtered == network), 2); %Find the number of vertices for $network in IPSILATERAL hem 
    labels2_N_idx_count = size(indices2(labels2_filtered == network), 2); %Find the number of vertices for $network in CONTRALATERAL hemi
    
    %find number of vertices assigned to network N in hemispheres
    %(normalizing for SA)
    network_N_ipsi_total = sum(labels.lh_labels == network);  %LH = ipsilateral
    network_N_contra_total = sum(labels.rh_labels == network); %RH = contralateral
        
    %calculate AI using network N indices
    AI_left = (labels1_N_idx_count/network_N_ipsi_total) - (labels2_N_idx_count/network_N_contra_total);
	fprintf('Processing...');
end


%Function to identify frequent network contribution for each vertex: RH
function [AI_right] = ai_right(x, labels, network)
	clmns = numel(x);   % fsaverage6 space, so each hemi is same number of vertices
	indices2 = find(x(1:(clmns/2))>= abs(0.25)); % Contralateral: Find indices of columns with correlation >= 0.25 for part 1  
	indices1 = find(x((clmns/2 +1):round(end))>= abs(0.25)); % Ipsilateral: Find indices of columns with correlation >= 0.25 for part 2
	labels1_filtered = labels.rh_labels(indices1); % Filtered labels for part 1 - Ipsilateral 
	labels2_filtered = labels.lh_labels(indices2); % Filtered labels for part 2

    %find number of highly correlated vertices with labels of $network
    labels1_N_idx_count = size(indices1(labels1_filtered == network), 2); %Find the number of vertices for $network in IPSILATERAL hem 
    labels2_N_idx_count = size(indices2(labels2_filtered == network), 2); %Find the number of vertices for $network in CONTRALATERAL hemi
    
    %find number of vertices assigned to network N in hemispheres
    %(normalizing for SA)
    network_N_ipsi_total = sum(labels.rh_labels == network);  %RH = ipsilateral
    network_N_contra_total = sum(labels.lh_labels == network); %LH = contralateral
        
    %calculate AI using network N indices
    AI_right = (labels1_N_idx_count/network_N_ipsi_total) - (labels2_N_idx_count/network_N_contra_total);
	fprintf('Processing...');
end
