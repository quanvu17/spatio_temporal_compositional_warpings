# Reproducible code
# Copyright (c) 2022 Quan Vu
# Author: Quan Vu, quanv (at) uow.edu.au
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# Load packages, functions, and environment
source("scripts/utils.R")

## Compare results for dataset 1
results <- list()
for (i in 1:30){
  load(paste0("results/simulation_study_dataset1/summary_", i, ".rda"))
  results[[i]] <- unlist(summary_results)
}
results <- matrix(unlist(results), nrow = 30, byrow = T)
## Results (as in the paper)
matrix(round(colMeans(results), 3), ncol = 2)

## Compare results for dataset 2
results <- list()
for (i in 1:30){
  load(paste0("results/simulation_study_dataset2/summary_", i, ".rda"))
  results[[i]] <- unlist(summary_results)
}
results <- matrix(unlist(results), nrow = 30, byrow = T)
## Results (as in the paper)
matrix(round(colMeans(results), 3), ncol = 2)
