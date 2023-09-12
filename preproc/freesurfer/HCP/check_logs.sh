#!/bin/bash

#Purpose: This script loops through each output file and looks for phrase "without error" to indicate that freesurfer completed successfully. Returns a list of subjects that failed.

#PATHS
CODE_DIR=/fslgroup/fslg_spec_networks/compute/code/HCP_analysis/freesurfer

#LOOP
for file in ${CODE_DIR}/logfiles/*.txt; do
    if grep -q "without error" "$file"; then
        echo "$file completed successfully."
    else
        echo "Error: $file" >> $CODE_DIR/failed_subs.txt
    fi
done
