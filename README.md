# Constructing Large Nonstationary Spatio-Temporal Covariance Models via Compositional Warpings

This repository contains reproducible code for the manuscript *Constructing Large Nonstationary Spatio-Temporal Covariance Models via Compositional Warpings* by Quan Vu, Andrew Zammit-Mangion and Stephen J. Chuter.

## Instructions

To reproduce the results and the figures in the manuscript, please download this repository.

Ensure the required packages are installed. Please ensure to set up a conda environment with TensorFlow v1.15 to run the TensorFlow code, and modify the file `scripts/utils.R` to load the correct environment.

The code to reproduce the results of the manuscript is saved in the folder `scripts/`, and the code to reproduce the figures is saved in the folder `scripts_plot/`. Once the repository is downloaded, run the bash file `run.sh` to reproduce the results in the manuscript. To reproduce the figures in the manuscript, please run the bash file `run_plot.sh`. The results in the manuscript are saved in the folders `results/` while the figures used in the manuscript are saved in the folder `figures/`.
