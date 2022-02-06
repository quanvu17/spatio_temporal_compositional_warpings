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

##############
load("results/application_PIG_grounding_line.rda")
load("results/application/allyears_statsep_predictions.rda")
plot_t <- list()

for (i in 1:6){
  year <- 2011 + i
  pred_t <- pred$df_pred[pred$df_pred$t > (0.2 * i - 0.1) & pred$df_pred$t < (0.2 * i + 0.1),]
  
  plot_t[[i]] <- ggplot() +
    ggtitle(year) +
    geom_tile(data = pred_t, aes(s1_sc * 50, s2_sc * 50, fill = sqrt(pred_var)), show.legend = T) +
    scale_fill_distiller(palette = "BrBG", # BrBG
                         direction = -1, limits = c(0, 0.25),
                         oob = squish, name = paste0("SE (m/yr)")) +
    geom_point(data = PIG_line_polarstereo, aes(s1, s2), size = 0.5) +
    theme_bw() + coord_fixed() + theme(text = element_text(size=15)) + 
    labs(x = 'Polar Stereographic Dist. x (km)',
         y = 'Polar Stereographic Dist. y (km)')
  
}

load("results/application/allyears_nonstatsep_predictions.rda")
plot_t_NS <- list()

for (i in 1:6){
  year <- 2011 + i
  pred_t <- pred$df_pred[pred$df_pred$t > (0.2 * i - 0.1) & pred$df_pred$t < (0.2 * i + 0.1),]
  
  plot_t_NS[[i]] <- ggplot() +
    ggtitle(year) +
    geom_tile(data = pred_t, aes(s1_sc * 50, s2_sc * 50, fill = sqrt(pred_var)), show.legend = T) +
    scale_fill_distiller(palette = "BrBG", # BrBG
                         direction = -1, limits = c(0, 0.25),
                         oob = squish, name = paste0("SE (m/yr)")) +
    geom_point(data = PIG_line_polarstereo, aes(s1, s2), size = 0.5) +
    theme_bw() + coord_fixed() + theme(text = element_text(size=15)) + 
    labs(x = 'Polar Stereographic Dist. x (km)',
         y = 'Polar Stereographic Dist. y (km)')
  
}

full.plot <- grid.arrange(plot_t[[1]], plot_t[[2]], plot_t[[3]],
                          plot_t_NS[[1]], plot_t_NS[[2]], plot_t_NS[[3]],
                          nrow = 2)

ggsave(filename = "figures/application_se.png",
       plot = full.plot, width = 36, height = 24, units = "cm")
