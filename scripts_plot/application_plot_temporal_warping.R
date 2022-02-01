# Load source
source("scripts/utils.R")

## Temporal warpings
load("results/application/allyears_nonstatsep_warpings.rda")

eta_t <- warping_param[[2]]
layers_temp <- c(AWU(r = 50L, dim = 1L, grad = 5, lims = c(-0.5, 0.5)))
nlayers_temp <- length(layers_temp)

t <- seq(2012, 2017, length.out = 101)
twarped <- as.matrix((t - 2011)/5)
twarped <- twarped %>% scale_fn1(warping_param[[4]][[1]])
for(j in 1: (nlayers_temp)){
  twarped <- layers_temp[[j]]$fR(twarped, eta_t[[j]]) %>% scale_fn1(warping_param[[4]][[2]])
}

df_t <- data.frame(t = t, tw = twarped)

plot <- ggplot() +
  geom_line(data = df_t, aes(t, tw)) +
  theme_bw() + theme(text = element_text(size=15)) +
  labs(x = "t", y = expression(paste(f['t'],'(', t, ')')))

ggsave(filename = "figures/application_temporal_warpings.png", plot = plot,
       width = 10, height = 10, units = "cm")
