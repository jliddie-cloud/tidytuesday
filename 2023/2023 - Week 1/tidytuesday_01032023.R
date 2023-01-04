################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 01-03-2023
################################################################################

library(tidyverse)
library(spdep)
library(sf)
library(gganimate)
library(ggtext)
library(showtext)

HUCs <- st_read("/Users/jahredl/Dropbox/Centralized datasets/huc250k_shp/huc250k_shp/huc250k.shp")

# remove great lakes for visual clarity later
HUCs <- subset(HUCs, HUC_NAME != "Lake Superior" &
                     HUC_NAME != "Lake Erie" &
                     HUC_NAME != "Lake Huron" &
                     HUC_NAME != "Lake Michigan")


# generate neighbor matrix using queen's contiguity
nb <- poly2nb(HUCs, queen = TRUE)
lw <- nb2listw(nb, zero.policy = TRUE)

# weights matrix to populate
weights.matrix <- matrix(nrow = nrow(HUCs), ncol = nrow(HUCs))

# convert to weights matrix
for (i in seq_along(weights.matrix[1,])) {
  
  weights.matrix[i, i] <- 0 # zero on diagonal
  
  # spatial weights for all observations
  weights.matrix[i, c(unlist(nb[[i]]))] <- 1/length(c(unlist(nb[[i]])))
  
  # set all others to zero
  weights.matrix[i, -c(unlist(nb[[i]]))] <- 0
  
}

# make those without any neighbors equal to 0 for all cells  
weights.matrix[is.na(weights.matrix)] <- 0

HUCs <- HUCs %>% select(HUC_CODE)

data_generation.f <- function(lambda = NULL, b0 = 0, sdy = NULL,
                              newcolumn = NULL, dataset = HUCs) {
  
  # random normal variable with mean y = b0, std dev = sdy
  y.nonspatial <- rnorm(n = nrow(HUCs), 
                        mean = b0,
                        sd = sdy)
  
  y.hat <- b0
  
  # solve for u: u = (1 - lambda * W)^-1 %*% epsilon (where epsilon is non-spatial deviation from mean)
  u <- solve(diag(nrow(HUCs)) - lambda * weights.matrix) %*% (y.nonspatial - y.hat)
  
  u <- as.numeric(u)
  
  y.spatial <- NA
  
  # add in spatially autocorrelated error using weights matrix
  for (j in 1:nrow(HUCs)) {
    
    y.spatial[j] <- y.nonspatial[j] + lambda * weights.matrix[j,] %*% u
    
  }
  
  # rename and format final variable
  spatial.variable <- data.frame(y.spatial = y.spatial)
  
  spatial.variable <- spatial.variable %>%
    mutate(!!newcolumn := y.spatial)
  
  spatial.variable <- spatial.variable %>% select(-y.spatial)
  
  return(spatial.variable)
  
}

lambdas <- seq(0, 0.9, by = 0.1)

var.names <- paste("lambda_", lambdas, sep = "")

dat <- data.frame(HUC_CODE = HUCs$HUC_CODE)

set.seed(01032023)

# now map over lambdas
dat <- map2_dfc(lambdas, var.names,
                ~data_generation.f(lambda = .x, sdy = 10, 
                                   newcolumn = .y, dataset = HUCs)
)

HUC_long <- cbind(HUCs, dat)

HUC_long <- pivot_longer(HUC_long, cols = lambda_0:lambda_0.9,
                         names_to = "spatial_autocorr",
                         values_to = "value")

# test facet plot
ggplot(HUC_long %>% filter(spatial_autocorr == "lambda_0" | spatial_autocorr == "lambda_0.5" | 
                           spatial_autocorr == "lambda_0.9")) + 
  geom_sf(aes(fill = value), linewidth = 0.01, color = "grey") +
  labs(title = "Simulation of a random spatially autocorrelated variable",
       caption = "Visualization by @jmliddie | #tidytuesday week 1") +
  scale_fill_gradientn(colors = rev(MetBrewer::met.brewer("Benedictus")),
                       breaks = c(-60, 0, 60), name = "",
                       labels = c("More negative", "", "More positive")) +
  ggthemes::theme_map() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.key.height = unit(0.3, 'cm'),
    legend.key.width= unit(1, 'cm'),
    legend.title = element_text(size = 8, color = "white"),
    plot.title = element_text(hjust = 0, size = 18, 
                              face = "bold", color = "white"),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),
    plot.subtitle = element_text(hjust = 0, size = 10, 
                                 lineheight = 2, color = "white"),
    plot.caption = element_text(hjust=0, color = "white", 
                                lineheight = 2, size = 5),
    plot.background = element_rect(fill = "black")) +
  guides(fill = guide_colourbar(ticks = FALSE, title.position = "top")) +
  facet_grid(~spatial_autocorr)

font_add_google("Roboto") # plot title
showtext_auto(enable = TRUE)

# now animate it
anim.HUC <-
  ggplot(HUC_long) + 
  geom_sf(aes(fill = value), linewidth = 0.01, color = "grey") +
  labs(title = "Spatially autocorrelated data",
       subtitle = "Simulations of increasing spatial autocorrelation in\nwatershed units of the US",
       caption = "Visualization by @jmliddie | #tidytuesday 2023 week 1 | Polygons: USGS") +
  scale_fill_gradientn(colors = rev(MetBrewer::met.brewer("Benedictus")),
                       breaks = c(-60, 0, 60), name = "",
                       labels = c("More negative", "", "More positive")) +
  ggthemes::theme_map() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.key.height = unit(0.3, 'cm'),
    legend.key.width = unit(1, 'cm'),
    plot.title = element_text(hjust = 0.5, size = 15, family = "Roboto",
                              color = "white", margin = margin(t = 6, b = 6)),
    plot.subtitle = element_text(hjust = 0.5, size = 8, family = "Roboto",
                                 color = "white"),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),
    plot.caption = element_text(hjust = 0, color = "white", lineheight = 2, 
                                size = 4),
    plot.background = element_rect(fill = "black")) +
  guides(fill = guide_colourbar(ticks = FALSE, title.position = "top")) +
  transition_manual(spatial_autocorr)
  
# getting font warnings but seems to be working...now save
animate(anim.HUC, nframes = 400, fps = 30, width = 5, height = 4, units = "in",
        res = 300, bg = "black",
        renderer = gifski_renderer("2023/2023 - Week 1/sim_watersheds.gif"))

