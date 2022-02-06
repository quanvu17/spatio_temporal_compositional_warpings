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

## The original data files are too large, so they were pre-processed first 
## If you require the original data files, please contact the author

# Load clean data
load("results/application_clean_data.rda")

### Scale data
df$s1_sc <- df$s1 / 50
df$s2_sc <- df$s2 / 50
df$t_sc <- (df$t - 2011) / 5

### Random missing in all years
set.seed(1)
sam <- sample(1:nrow(df), 0.8 * nrow(df))
df_train <- df[sam,]
df_test <- setdiff(df, df_train)

# Set up warping layers
layers_spat <- c(RBF_block(res = 1L),
                 RBF_block(res = 1L))
layers_temp <- c(AWU(r = 50L, dim = 1L, grad = 5, lims = c(-0.5, 0.5)))

# Set up order and neighbor
locs <- cbind(df_train$s1_sc, df_train$s2_sc)
locs_t <- cbind(df_train$s1_sc, df_train$s2_sc, df_train$t_sc)
# Order by max-min ordering in space
order_id <- order_maxmin(locs)
# Nearest neighbor in space and time
nn_id <- find_ordered_nn(locs_t[order_id,], m = 50, st_scale = c(1, 1))

# Fit Models
## Stationary, Separable model
d <- deepspat_nn_ST_GP(f = z ~ s1_sc + s2_sc + t_sc - 1, data = df_train, g = ~ 1,
                       family = "exp_stat_sep",
                       layers_spat = layers_spat, layers_temp = layers_temp,
                       m = 50L,
                       order_id = order_id, nn_id = nn_id,
                       method = "REML", nsteps = 100L,
                       par_init = initvars(l_top_layer = 1),
                       learn_rates = init_learn_rates(eta_mean = 0.01)
)

# Predictions
newdata <- rbind(df_test, df_train)
locs_new <- cbind(newdata$s1_sc, newdata$s2_sc, newdata$t_sc)
nn_id_pred <- FNN::get.knnx(data = locs_t, query = locs_new, k = 50)$nn.index
pred <- predict.deepspat_nn_ST_GP(d, newdata, nn_id_pred)
save(pred, file = "results/application/allyears_statsep_predictions.rda")

pred_mean <- pred$df_pred$pred_mean[1:nrow(df_test)]
pred_var <- pred$df_pred$pred_var[1:nrow(df_test)] + 1/d$run(d$precy_tf)
pred_95l <- pred_mean - 1.96 * sqrt(pred_var)
pred_95u <- pred_mean + 1.96 * sqrt(pred_var)

RMSPE_statsep <- RMSPE(df_test$z, pred_mean)
CRPS_statsep <- CRPS(df_test$z, pred_mean, pred_var)
IS95_statsep <- IS95(df_test$z, pred_95l, pred_95u)
CVG_statsep <- CVG(df_test$z, pred_95l, pred_95u)

## Nonstationary, Separable model
d <- deepspat_nn_ST_GP(f = z ~ s1_sc + s2_sc + t_sc - 1, data = df_train, g = ~ 1,
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
save(warping_param, file = "results/application/allyears_nonstatsep_warpings.rda")

# Predictions
newdata <- rbind(df_test, df_train)
locs_new <- cbind(newdata$s1_sc, newdata$s2_sc, newdata$t_sc)
nn_id_pred <- FNN::get.knnx(data = locs_t, query = locs_new, k = 50)$nn.index
pred <- predict.deepspat_nn_ST_GP(d, newdata, nn_id_pred)
save(pred, file = "results/application/allyears_nonstatsep_predictions.rda")

pred_mean <- pred$df_pred$pred_mean[1:nrow(df_test)]
pred_var <- pred$df_pred$pred_var[1:nrow(df_test)] + 1/d$run(d$precy_tf)
pred_95l <- pred_mean - 1.96 * sqrt(pred_var)
pred_95u <- pred_mean + 1.96 * sqrt(pred_var)

RMSPE_nonstatsep <- RMSPE(df_test$z, pred_mean)
CRPS_nonstatsep <- CRPS(df_test$z, pred_mean, pred_var)
IS95_nonstatsep <- IS95(df_test$z, pred_95l, pred_95u)
CVG_nonstatsep <- CVG(df_test$z, pred_95l, pred_95u)

summary_results <- list(RMSPE_statsep, RMSPE_nonstatsep,
                        CRPS_statsep, CRPS_nonstatsep,
                        IS95_statsep, IS95_nonstatsep,
                        CVG_statsep, CVG_nonstatsep)

save(summary_results,
     file = paste0("results/application/allyears_summary.rda"))
