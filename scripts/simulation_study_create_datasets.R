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

# Simulate data for the experiment

# Dataset 1. From nonstationary, separable covariance
# Simulate warped location
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
swarped <- s
for(j in 1: (nlayers)) {
  swarped <- layers[[j]]$fR(swarped, eta[[j]]) %>% scal_0_5_mat()
}

layers_temp <- c(AWU(r = 50L, dim = 1L, grad = 200, lims = c(-0.5, 0.5)))
nlayers_temp <- length(layers_temp)
eta_t <- list()
for(j in 1:nlayers_temp) {
  eta_t[[j]] <- c(rep(0.2, 15), rep(0.5, 10), rep(0.7, 10), rep(0.2, 15))
}

twarped <- t <- as.matrix(seq(-0.5, 0.5, length.out = 10))
for(j in 1: (nlayers_temp)){
  twarped <- layers_temp[[j]]$fR(twarped, eta_t[[j]]) %>% scal_0_5_mat()
}

warped <- cbind(matrix(c(rep(swarped[,1], length(twarped)), rep(swarped[,2], length(twarped))), ncol = 2),
                rep(twarped, each = nrow(swarped)))
locs <- cbind(matrix(c(rep(s[,1], length(t)), rep(s[,2], length(t))), ncol = 2),
                rep(t, each = nrow(s)))

# Simulate stationary covariance on the warped domain
set.seed(3)

D_s <- fields::rdist(warped[,1:2])
D_t <- fields::rdist(warped[,3])
C <- Cov_sep(D_s, D_t, sigma2 = 1, alpha = 10, alpha_t = 5)
K <- t(chol(C))
y <- K %*% rnorm(nrow(K))
z <- y + 0.2 * rnorm(length(y))
df <- data.frame(locs[,1], locs[,2], locs[,3], warped[,1], warped[,2], warped[,3], y, z)
names(df) <- c("s1", "s2", "t", "sw1", "sw2", "tw", "y", "z")

save(df, file="results/simulation_study_dataset1/dataset.rda")

##############################################

# Dataset 2. Asymmetric + nonstationary in space, stationary in time
# Simulate warped location
set.seed(11)
layers <- c(RBF_block(res = 1L))
nlayers <- length(layers)
eta <- list()
for(j in 1:(nlayers)) eta[[j]] <- runif(n = 1, min = -0.5, max = 1)

s <- expand.grid(seq(-0.5, 0.5, length.out = 51),
                 seq(-0.5, 0.5, length.out = 51)) %>% as.matrix()
swarped <- s
for(j in 1: (nlayers)) {
  swarped <- layers[[j]]$fR(swarped, eta[[j]]) %>% scal_0_5_mat()
}
twarped <- t <- as.matrix(seq(-0.5, 0.5, length.out = 10))

warped <- cbind(matrix(c(rep(swarped[,1], length(twarped)), rep(swarped[,2], length(twarped))), ncol = 2),
                rep(twarped, each = nrow(swarped)))
locs <- cbind(matrix(c(rep(s[,1], length(t)), rep(s[,2], length(t))), ncol = 2),
              rep(t, each = nrow(s)))

# Simulate stationary covariance on the warped domain
set.seed(3)

C <- Cov_asym(s_1 = warped[,1], s_2 = warped[,2], t = warped[,3],
                     v = c(-0.3, 0), alpha = 10, sigma2 = 1)

K <- t(chol(C + 0.001*diag(nrow(C))))
y <- K %*% rnorm(nrow(K))
z <- y + 0.2 * rnorm(length(y))
df <- data.frame(locs[,1], locs[,2], locs[,3], warped[,1], warped[,2], warped[,3], y, z)
names(df) <- c("s1", "s2", "t", "sw1", "sw2", "tw", "y", "z")

save(df, file="results/simulation_study_dataset2/dataset.rda")
