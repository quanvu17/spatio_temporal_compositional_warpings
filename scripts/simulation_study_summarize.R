# Load source
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
boxplot(results)

## Compare results for dataset 2
results <- list()
for (i in 1:30){
  load(paste0("results/simulation_study_dataset2/summary_", i, ".rda"))
  results[[i]] <- unlist(summary_results)
}
results <- matrix(unlist(results), nrow = 30, byrow = T)
## Results (as in the paper)
matrix(round(colMeans(results), 3), ncol = 2)
boxplot(results)