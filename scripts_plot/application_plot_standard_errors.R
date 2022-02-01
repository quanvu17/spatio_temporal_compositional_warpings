## Plot predictions
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
