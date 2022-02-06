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

## Load conda environment
## Modify this line of code to load your own environment
reticulate::use_condaenv("TFv1-15", required=TRUE)

# Load packages
library("colorspace")
library("data.table")
library("dplyr")
library("devtools") ## for building deepspat
library("fields")
library("GpGp")
library("ggplot2")
library("ggquiver")
library("gridExtra")
library("ncdf4")
library("RandomFields")
library("rgdal")
library("scales")
library("verification")
load_all("deepspat")


# RMSPE
RMSPE <- function(true, pred){
  sqrt(mean((true - pred)^2))
}

# CRPS
CRPS <- function(true, pred, pred_var){
  crps(true, cbind(pred, sqrt(pred_var)))$CRPS
}

# IS
IS95 <- function(true, pred95l, pred95u) {
  alpha = 0.05
  ISs <- (pred95u - pred95l) + 2/alpha * (pred95l - true) * (true < pred95l) +
    2/alpha * (true - pred95u) * (true > pred95u)
  mean(ISs)
}

# CVG
CVG <- function(true, pred95l, pred95u) {
  coverage <- (true > pred95l) * (true < pred95u)
  mean(coverage)
}

## Separable covariance
Cov_sep <- function(D_s, D_t, sigma2, alpha, alpha_t){
  K <- sigma2 * exp(-alpha * D_s) * exp(-alpha_t * D_t)
  return(K)
}

## Asymmetric covariance
Cov_asym <- function(s_1, s_2, t, v, sigma2, alpha){
  ## v is a 2-dimensional vector
  s <- cbind(s_1, s_2)
  vt <- matrix(v, ncol = 1) %*% matrix(t, nrow = 1)
  ## s - vt
  s_vt <- s - t(vt)
  D <- fields::rdist(s_vt)
  K <- sigma2 * exp(-alpha * D)
  return(K)
}

## Scale function (for the warpings)
scale_fn <- function(sw, scalings){
  cbind((sw[,1] - scalings$min[1]) / (scalings$max[1] - scalings$min[1]) - 0.5,
        (sw[,2] - scalings$min[2]) / (scalings$max[2] - scalings$min[2]) - 0.5)
}

scale_fn1 <- function(sw, scalings){
  (sw - scalings$min[1]) / (scalings$max[1] - scalings$min[1]) - 0.5
}
