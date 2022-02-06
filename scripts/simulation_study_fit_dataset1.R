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

# Load dataset 1
load("results/simulation_study_dataset1/dataset.rda")

# Train data and test data
i = commandArgs(trailingOnly=TRUE)
i <- as.numeric(i)
set.seed(i)
sam <- sample(1:nrow(df), 0.8 * nrow(df))
df_train <- df[sam,]
df_test <- setdiff(df, df_train)

# Set up warping layers
layers_spat <- c(RBF_block(res = 1L),
                 RBF_block(res = 1L))
layers_temp <- c(AWU(r = 50L, dim = 1L, grad = 200, lims = c(-0.5, 0.5)))

# Set up order and neighbor
locs <- t(rbind(df_train$s1, df_train$s2))
locs_t <- t(rbind(df_train$s1, df_train$s2, df_train$t))
# Order by max-min ordering in space
order_id <- order_maxmin(locs)
# Nearest neighbor in space and time
nn_id <- find_ordered_nn(locs_t[order_id,], m = 50)

# Fit Models
## Stationary, Separable model
d <- deepspat_nn_ST_GP(f = z ~ s1 + s2 + t - 1, data = df_train, g = ~ 1,
                       family = "exp_stat_sep",
                       layers_spat = layers_spat, layers_temp = layers_temp,
                       m = 50L,
                       order_id = order_id, nn_id = nn_id,
                       method = "REML", nsteps = 200L,
                       par_init = initvars(l_top_layer = 0.5),
                       learn_rates = init_learn_rates(eta_mean = 0.01)
)

# Predictions
newdata <- rbind(df_test, df_train)
locs_new <- t(rbind(newdata$s1, newdata$s2, newdata$t))
nn_id_pred <- FNN::get.knnx(data = locs_t, query = locs_new, k = 50)$nn.index
pred <- predict.deepspat_nn_ST_GP(d, newdata, nn_id_pred)
RMSPE_statsep <- RMSPE(df_test$y, pred$df_pred$pred_mean[1:nrow(df_test)])
CRPS_statsep <- CRPS(df_test$y,
                     pred$df_pred$pred_mean[1:nrow(df_test)],
                     pred$df_pred$pred_var[1:nrow(df_test)])

if (i == 1) {
  save(pred, file = "results/simulation_study_dataset1/statsep_predictions.rda")
}

## Nonstationary, Separable model
d <- deepspat_nn_ST_GP(f = z ~ s1 + s2 + t - 1, data = df_train, g = ~ 1,
                       family = "exp_nonstat_sep",
                       layers_spat = layers_spat, layers_temp = layers_temp,
                       m = 50L,
                       order_id = order_id, nn_id = nn_id,
                       method = "REML", nsteps = 200L,
                       par_init = initvars(l_top_layer = 0.5),
                       learn_rates = init_learn_rates(eta_mean = 0.01)
)

# Save fitted warping parameters
eta <- d$run(d$eta_tf)
eta_t <- d$run(d$eta_t_tf)
scalings <- d$run(d$scalings)
scalings_t <- d$run(d$scalings_t)
swarped_tf <- d$run(d$swarped_tf)
twarped_tf <- d$run(d$twarped_tf)
warping_param <- list(eta, eta_t, scalings, scalings_t, swarped_tf, twarped_tf)
if (i == 1){
  save(warping_param, file = "results/simulation_study_dataset1/nonstatsep_warpings.rda")
}

# Predictions (with NNs on G)
newdata <- rbind(df_test, df_train)
locs_new <- t(rbind(newdata$s1, newdata$s2, newdata$t))
nn_id_pred <- FNN::get.knnx(data = locs_t, query = locs_new, k = 50)$nn.index
pred <- predict.deepspat_nn_ST_GP(d, newdata, nn_id_pred)
RMSPE_nonstatsepG <- RMSPE(df_test$y, pred$df_pred$pred_mean[1:nrow(df_test)])
CRPS_nonstatsepG <- CRPS(df_test$y,
                     pred$df_pred$pred_mean[1:nrow(df_test)],
                     pred$df_pred$pred_var[1:nrow(df_test)])

if (i == 1){
  save(pred, file = "results/simulation_study_dataset1/nonstatsep_G_predictions.rda")
}

# Predictions (with NNs on D)
newdata <- rbind(df_test, df_train)
locs_warped <- cbind(swarped_tf[,1], swarped_tf[,2], twarped_tf)

swarped <- cbind(newdata$s1, newdata$s2) %>%
  scale_fn(scalings[[1]])
for(j in 1:(length(layers_spat))) {
  swarped <- layers_spat[[j]]$fR(swarped, as.numeric(eta[[j]])) %>%
    scale_fn(scalings[[j + 1]])
}

twarped <- as.matrix(newdata$t %>% scale_fn1(scalings_t[[1]]))
for(j in 1:(length(layers_temp))) {
  twarped <- layers_temp[[j]]$fR(twarped, as.numeric(eta_t[[j]])) %>%
    scale_fn1(scalings_t[[j + 1]])
}

locs_new_warped <- cbind(swarped[,1], swarped[,2], twarped)
nn_id_pred <- FNN::get.knnx(data = locs_warped, query = locs_new_warped, k = 50)$nn.index
pred <- predict.deepspat_nn_ST_GP(d, newdata, nn_id_pred)
RMSPE_nonstatsepD <- RMSPE(df_test$y, pred$df_pred$pred_mean[1:nrow(df_test)])
CRPS_nonstatsepD <- CRPS(df_test$y,
                         pred$df_pred$pred_mean[1:nrow(df_test)],
                         pred$df_pred$pred_var[1:nrow(df_test)])

if (i == 1) {
  save(pred, file = "results/simulation_study_dataset1/nonstatsep_D_predictions.rda")
}

summary_results <- list(RMSPE_statsep, RMSPE_nonstatsepG, RMSPE_nonstatsepD,
                        CRPS_statsep, CRPS_nonstatsepG, CRPS_nonstatsepD)
save(summary_results,
     file = paste0("results/simulation_study_dataset1/summary_", i, ".rda"))
