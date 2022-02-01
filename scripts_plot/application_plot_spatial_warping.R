# Load source
source("scripts/utils.R")

#####################################
load("results/application_PIG_grounding_line.rda")
load("results/application/allyears_nonstatsep_warpings.rda")
load("results/application_clean_data.rda")
load("results/application_bed.rda")

## Bedrock

plot_bedrock <- ggplot() +
  geom_point(data = bed_df, aes(s1, s2, color = bed), show.legend = T) +
  scale_color_distiller(palette = "Spectral", name = 'Bedrock (m)', direction = 1,
                        limits = c(-1800, 1100)) +
  # geom_point(data = PIG_line_polarstereo, aes(s1, s2), size = 0.5) +
  theme_bw() + coord_fixed() + theme(text = element_text(size=15)) +
  labs(x = 'Polar Stereographic Dist. x (km)',
       y = 'Polar Stereographic Dist. y (km)')

eta <- warping_param[[1]]
scalings <- warping_param[[3]]
swarped_est <- warping_param[[5]]

## Finding the grounding line on the warped domain
layers_spat <- c(RBF_block(res = 1L),
                 RBF_block(res = 1L))
swarped <- cbind(PIG_line_polarstereo$s1/50, PIG_line_polarstereo$s2/50) %>% scale_fn(scalings[[1]])
for(j in 1: (length(layers_spat))) {
  swarped <- layers_spat[[j]]$fR(swarped, as.numeric(eta[[j]])) %>% scale_fn(scalings[[j + 1]])
}

PIG_line_warped <- data.frame(s1 = swarped[,1], s2 = swarped[,2])

df_t <- df %>% filter(t == 2014) 
swarped <- cbind(df_t$s1 / 50, df_t$s2 / 50) %>% scale_fn(scalings[[1]])
for(j in 1:(length(layers_spat))) {
  swarped <- layers_spat[[j]]$fR(swarped, as.numeric(eta[[j]])) %>% scale_fn(scalings[[j + 1]])
}

plot_original <- ggplot() +
  geom_point(data = df_t, aes(s1, s2,
                              color = z), size = 0.5, show.legend = T) +
  scale_color_continuous_divergingx(palette = "RdBu",
                                    name = expression(paste(Delta, 'h / ', Delta, 't (m/yr)')),
                                    limits = c(-4,2), mid = 0) +
  geom_point(data = PIG_line_polarstereo, aes(s1, s2), size = 0.5) +
  theme_bw() + coord_fixed() + theme(text = element_text(size=15)) +
  labs(x = 'Polar Stereographic Dist. x (km)',
       y = 'Polar Stereographic Dist. y (km)')

plot_warped <- ggplot() +
  geom_point(data = df_t, aes(swarped[,1], swarped[,2],
                              color = z), size = 0.5, show.legend = T) +
  scale_color_continuous_divergingx(palette = "RdBu", 
                                    name = expression(paste(Delta, 'h / ', Delta, 't (m/yr)')),
                                    limits = c(-4,2), mid = 0) +
  geom_point(data = PIG_line_warped, aes(s1, s2), size = 0.5) +
  theme_bw() + coord_fixed() + theme(text = element_text(size=15)) +
  labs(x = expression(paste(f[bold("s, 1")],"(", bold(s), ")")),
       y = expression(paste(f[bold("s, 2")],"(", bold(s), ")")))

#####################################

load("results/application_ice_velocity.rda")

a <- (0:15)*16 + 1
b <- (0:15)*16
idx <- rep(a, length(b)) + 256 * rep(b, each = length(a))

grid <- expand.grid(seq(-1708, -1453, 1), seq(-38, -317))
coord_polarstereo$s1 <- grid[,1]
coord_polarstereo$s2 <- grid[,2]
coord_polarstereo_new <- coord_polarstereo[idx,]

library(ggquiver)
plot_velocity <- ggplot(data = coord_polarstereo_new,
                        aes(x = s1, y = s2,
                            u = VX, v = VY)) +
  geom_quiver(vecsize = 2) + 
  theme_bw() + coord_fixed() + theme(text = element_text(size=15)) +
  labs(x = 'Polar Stereographic Dist. x (km)',
       y = 'Polar Stereographic Dist. y (km)')

plot_original <- ggplotGrob(plot_original)
plot_warped <- ggplotGrob(plot_warped)
plot_velocity <- ggplotGrob(plot_velocity)
plot_bedrock <- ggplotGrob(plot_bedrock)

plot_original$widths <- plot_warped$widths
plot_velocity$widths <- plot_warped$widths
plot_bedrock$widths <- plot_warped$widths

full.plot <- grid.arrange(plot_original, plot_warped,
                          plot_velocity, plot_bedrock,
                          nrow = 2)

ggsave(filename="figures/application_spatial_warpings.png", plot=full.plot,
       device="png", width=30, height=30, scale=1, units="cm", dpi=300)
