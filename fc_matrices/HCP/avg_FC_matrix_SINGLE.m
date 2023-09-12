% Purpose: Load FC matrices and create an average FC matrix for each participant.
% Inputs: FC matrices for individual rs-fMRI runs.
% Outputs: Single averaged FC matrix per individual.
%
% Written by M. Peterson, Nielsen Brain and Behavior Lab, under MIT License 2022

%To run: 
%	 1. Claim computing resources using salloc (ex: `salloc --mem-per-cpu 300G --time 48:00:00 --x11`)
%	 2. Load matlab module: `ml matlab/r2018b`
%	 3. Enter the command `LD_PRELOAD= matlab`


% Step  1: Create 3D matrix of corr. matrices
project_dir = '/fslgroup/grp_parc/compute/HCP_analysis/parc_output_fs6_HCP_ALL/quant_metrics/FC_matrices';
out_dir = '/fslgroup/fslg_csf_autism/compute/HCP_analysis/parc_output_fs6_HCP_ALL/quant_metrics/FC_matrices';
sublist = ["SUB"];
seslist = ["1" "2" "3" "4"];

if(~exist(out_dir))
        mkdir(out_dir);
end

for i = sublist
    count=0;
    partial_file=fullfile(project_dir, strcat(i,'_sess-*.gz'));
    %try gunzip(partial_file);
	for ses = seslist
        %Create 3D matrix of all runs for a single subject
        filename=strcat(i,'_sess-',ses,'_fullcorr.mat');
   	    str = fullfile(project_dir,filename);
        if isfile(str)
            count=(count+1);
            new = load(str);
            if count==1
                matrix_1 = new.corr_mat; %name of struct for FC matrix
            else    
                matrix_1 = cat(3,matrix_1,new.corr_mat); 
            end
        else
        end
    end 
    
	%Created the individual-averaged FC matrix
	ind_avg = mean(matrix_1,3);
	file_out = strcat(i,'_avg_FC.mat');
    file_full = fullfile(out_dir,file_out);
    filename = sprintf(file_full, ind_avg);
    save(filename, 'ind_avg', '-v7.3');


    
	%Step 3: Plot the individual-averaged FC matrix
	%FC_plot = imagesc(ind_avg);
    %image_out = strcat('sub-',i,'_avg_FC.png');
    %full_image = fullfile(project_dir,image_out);
    %saveas(FC_plot, full_image, 'png');
    %end 
end

