# Load source
source("scripts/utils.R")

## Plot the data sets
load("results/application_clean_data.rda")
load("results/application_PIG_grounding_line.rda")

### Random missing in all years
set.seed(1)
sam <- sample(1:nrow(df), 0.8 * nrow(df))
df_train <- df[sam,]
df_test <- setdiff(df, df_train)

plot_t <- list()
for (i in 1:6){
  year <- 2011 + i
  df_t <- df_train %>% filter(t == year) 
  plot_t[[i]] <- ggplot() +
    ggtitle(year) +
    geom_point(data = df_t, aes(s1, s2, color = z), size = 1, show.legend = T) +
    scale_color_continuous_divergingx(palette = "RdBu",
                                      name = expression(paste(Delta, 'h / ', Delta, 't (m/yr)')),
                                      limits = c(-4,2), mid = 0) +
    geom_point(data = PIG_line_polarstereo, aes(s1, s2), size = 0.5) +
    theme_bw() + coord_fixed() + theme(text = element_text(size=15)) +
    labs(x = 'Polar Stereographic Dist. x (km)',
         y = 'Polar Stereographic Dist. y (km)')
}

plot <- grid.arrange(plot_t[[1]], plot_t[[2]], plot_t[[3]],
                     plot_t[[4]], plot_t[[5]], plot_t[[6]],
                     nrow = 2)

ggsave(filename = "figures/application_observations.png", plot = plot,
       width = 36, height = 24, units = "cm")