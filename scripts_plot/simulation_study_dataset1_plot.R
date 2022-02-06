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

# Dataset 1. From nonstationary, separable covariance
set.seed(16)
r1 <- 50
layers <- c(AWU(r = r1, dim = 1L, grad = 50, lims = c(-0.5, 0.5)),
            AWU(r = r1, dim = 2L, grad = 50, lims = c(-0.5, 0.5)),
            RBF_block(res = 1L))
nlayers <- length(layers)
eta <- list()
eta[[1]] <- sin(seq(0, pi, length.out = r1))
eta[[2]] <- c(1, rep(0, r1-1))
for(j in 3:(nlayers)) eta[[j]] <- runif(n = 1, min = -1, max = exp(3/2)/2)

s <- expand.grid(seq(-0.5, 0.5, length.out = 51),
                 seq(-0.5, 0.5, length.out = 51)) %>% as.matrix()
swarped_est <- swarped_true <- s
for(j in 1: (nlayers)) {
  swarped_true <- layers[[j]]$fR(swarped_true, eta[[j]]) %>% scal_0_5_mat()
}

layers <- c(RBF_block(res = 1L),
            RBF_block(res = 1L))
nlayers <- length(layers)
load("results/simulation_study_dataset1/nonstatsep_warpings.rda")
eta <- unlist(warping_param[[1]])
swarped_est <- swarped_est %>% scale_fn(warping_param[[3]][[1]])
for(j in 1: (nlayers)) {
  swarped_est <- layers[[j]]$fR(swarped_est, eta[j]) %>% scale_fn(warping_param[[3]][[j+1]])
}

sw_true <- data.frame(sw1 = swarped_true[,1], sw2 = swarped_true[,2])
sw_est <- data.frame(sw1 = swarped_est[,1], sw2 = swarped_est[,2])

plot01 <- ggplot(sw_true) +
  geom_point(aes(sw1, sw2), size = 0.5) +
  labs(x = expression(paste(f[bold("s, 1")],"(", bold(s), ")")),
       y = expression(paste(f[bold("s, 2")],"(", bold(s), ")"))) +
  theme_bw() + coord_fixed() + theme(text = element_text(size=15))

plot02 <- ggplot(sw_est) +
  geom_point(aes(sw1, sw2), size = 0.5) +
  labs(x = expression(paste(f[bold("s, 1")],"(", bold(s), ")")),
       y = expression(paste(f[bold("s, 2")],"(", bold(s), ")"))) +
  theme_bw() + coord_fixed() + theme(text = element_text(size=15))

layers_temp <- c(AWU(r = 50L, dim = 1L, grad = 200, lims = c(-0.5, 0.5)))
nlayers_temp <- length(layers_temp)
eta_t <- list()
for(j in 1:nlayers_temp) {
  eta_t[[j]] <- c(rep(0.2, 15), rep(0.5, 10), rep(0.7, 10), rep(0.2, 15))
}

twarped_est <- twarped_true <- t <- as.matrix(seq(-0.5, 0.5, length.out = 100))
for(j in 1: (nlayers_temp)){
  twarped_true <- layers_temp[[j]]$fR(twarped_true, eta_t[[j]]) %>% scal_0_5_mat()
}

layers_temp <- c(AWU(r = 50L, dim = 1L, grad = 200, lims = c(-0.5, 0.5)))
eta_t <- warping_param[[2]]
twarped_est <- twarped_est %>% scale_fn1(warping_param[[4]][[1]])
for(j in 1: (nlayers_temp)){
  twarped_est <- layers_temp[[j]]$fR(twarped_est, eta_t[[j]]) %>% scale_fn1(warping_param[[4]][[j+1]])
}

tw_true <- data.frame(t = t, tw = twarped_true)
tw_est <- data.frame(t = t, tw = twarped_est)

plot03 <- ggplot(tw_true) +
  geom_line(aes(t, tw)) +
  theme_bw() + theme(text = element_text(size=15)) +
  labs(x = "t", y = expression(paste(f['t'],'(', t, ')')))

plot04 <- ggplot(tw_est) +
  geom_line(aes(t, tw)) +
  theme_bw() + theme(text = element_text(size=15)) +
  labs(x = "t", y = expression(paste(f['t'],'(', t, ')')))

full.plot <- grid.arrange(plot01, plot02,
                          plot03, plot04,
                          nrow = 2)

ggsave(filename="figures/simulation_study_dataset1_warpings.png", plot=full.plot,
       device="png", width=20, height=20, scale=1, units="cm", dpi=300)
