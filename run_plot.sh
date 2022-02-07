#!/bin/bash

# Generate the figures
Rscript scripts_plot/application_plot_obs.R
Rscript scripts_plot/application_plot_spatial_warping.R
Rscript scripts_plot/application_plot_standard_errors.R
Rscript scripts_plot/application_plot_temporal_warping.R
Rscript scripts_plot/simulation_study_dataset1_plot.R
Rscript scripts_plot/simulation_study_dataset2_plot.R
Rscript scripts_plot/simulation_study_dataset2_plot_velocity.R
