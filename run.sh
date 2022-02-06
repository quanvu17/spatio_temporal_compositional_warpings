## Create datasets for simulation study
Rscript scripts/simulation_study_create_datasets.R

## Run simulation 1
for i in {1..30}
do
Rscript scripts/simulation_study_fit_dataset1.R $i
done

## Run simulation 2
for i in {1..30}
do
Rscript scripts/simulation_study_fit_dataset2.R $i
done

## summarize results for simulations
Rscript scripts/simulation_study_summarize.R

## Run case study
Rscript scripts/application_fit_allyears.R
