# Motion Metrics Scripts for the NSD Dataset

## Description
Scripts used to compute mean framewise displacement, mean DVARS, and volumes remaining following CBIG2016 preprocessing. Scripts are optimized for Python/3.6.

## Contents
1. rename_preproc_motion.sh #Used to rename surface-projected preproc output as consecutive sessions and copy to new location so as to align with 12 consecutive runs selected for FC Matrices
2. avg_FD.py #Used to calculate mean framewise displacement
3. avg_DVARS.py #Used to calculated mean DVARS
4. volumes_remaining.py #Used to calculate volumes available following censoring

## Additional Notes
These scripts were written for the even NSD sessions; highly similar scripts were used for the other iterations of the NSD dataset (e.g., odd, first half, random).
