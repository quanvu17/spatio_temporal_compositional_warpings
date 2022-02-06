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

######################################
## Comparison of warpings

# Dataset 2. From nonstationary, asymmetric covariance
set.seed(11)
layers <- c(RBF_block(res = 1L))
nlayers <- length(layers)
eta <- list()
for(j in 1:(nlayers)) eta[[j]] <- runif(n = 1, min = -0.5, max = 1)

s <- expand.grid(seq(-0.6, 0.6, length.out = 241),
                 seq(-0.6, 0.6, length.out = 241)) %>% as.matrix()
swarped_est <- swarped_true <- s
for(j in 1: (nlayers)) {
  swarped_true <- layers[[j]]$fR(swarped_true, eta[[j]]) %>% scal_0_5_mat()
}

layers <- c(RBF_block(res = 1L),
            RBF_block(res = 1L))
nlayers <- length(layers)
load("results/simulation_study_dataset2/nonstatasym_warpings.rda")
eta <- unlist(warping_param[[1]])
swarped_est <- swarped_est %>% scale_fn(warping_param[[3]][[1]])
for(j in 1: (nlayers)) {
  swarped_est <- layers[[j]]$fR(swarped_est, eta[j]) %>% scale_fn(warping_param[[3]][[j+1]])
}

sw_true <- data.frame(sw1 = swarped_true[,1], sw2 = swarped_true[,2])
sw_est <- data.frame(sw1 = swarped_est[,1], sw2 = swarped_est[,2])

####################################

s0 <- expand.grid(seq(-0.2, 0.6, length.out = 5),
                  seq(-0.5, 0.5, length.out = 6)) %>% as.matrix()

load("results/simulation_study_dataset2/statasym_warpings.rda")
df.s <- data.frame(s1 = s0[,1], s2 = s0[,2])

plot_velocity_stat <- ggplot(data = df.s,
                             aes(x = s1, y = s2,
                                 u = v_tf[1], v = v_tf[2])) +
  geom_quiver(vecsize = 0.5) + 
  theme_bw() + coord_cartesian(xlim = c(-0.35, 0.6), ylim = c(-0.55, 0.55)) + 
  theme(text = element_text(size=15)) +
  labs(x = expression(paste(s['1'])),
       y = expression(paste(s['2'])))

########

swarped_est <- s0

layers <- c(RBF_block(res = 1L),
            RBF_block(res = 1L))
nlayers <- length(layers)
load("results/simulation_study_dataset2/nonstatasym_warpings.rda")
eta <- unlist(warping_param[[1]])
swarped_est <- swarped_est %>% scale_fn(warping_param[[3]][[1]])
for(j in 1: (nlayers)) {
  swarped_est <- layers[[j]]$fR(swarped_est, eta[j]) %>% scale_fn(warping_param[[3]][[j+1]])
}

load("results/simulation_study_dataset2/nonstatasym_warpings.rda")
swarped_est[,1] <- swarped_est[,1] + warping_param[[7]][1]
swarped_est[,2] <- swarped_est[,2] + warping_param[[7]][2]

est_idx <- FNN::get.knnx(data = sw_est, query = swarped_est, k = 1)$nn.index

plot_velocity_nonstat_est <- ggplot(data = df.s,
                                    aes(x = s1, y = s2,
                                        u = s[est_idx, 1] - s1,
                                        v = s[est_idx, 2] - s2)) +
  geom_quiver(vecsize = 0.5) + 
  theme_bw() + coord_cartesian(xlim = c(-0.35, 0.6), ylim = c(-0.55, 0.55)) + 
  theme(text = element_text(size=15)) +
  labs(x = expression(paste(s['1'])),
       y = expression(paste(s['2'])))

###########

s_idx <- FNN::get.knnx(data = s, query = s0, k = 1)$nn.index

swarped_true <- sw_true[s_idx,]

swarped_true[,1] <- swarped_true[,1] - 0.3
swarped_true[,2] <- swarped_true[,2]

true_idx <- FNN::get.knnx(data = sw_true, query = swarped_true, k = 1)$nn.index

plot_velocity_nonstat_true <- ggplot(data = df.s,
                                     aes(x = s1, y = s2,
                                         u = s[true_idx, 1] - s1,
                                         v = s[true_idx, 2] - s2)) +
  geom_quiver(vecsize = 0.5) + 
  theme_bw() + coord_cartesian(xlim = c(-0.35, 0.6), ylim = c(-0.55, 0.55)) + 
  theme(text = element_text(size=15)) +
  labs(x = expression(paste(s['1'])),
       y = expression(paste(s['2'])))

############

full.plot <- grid.arrange(plot_velocity_stat, plot_velocity_nonstat_est, plot_velocity_nonstat_true,
                          nrow = 1)

ggsave(filename = "figures/simulation_study_dataset2_velocity.png",
       plot = full.plot, width = 30, height = 10, units = "cm")
