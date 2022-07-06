################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 07-05-2022
################################################################################
library(tidyverse)
library(ggmap)
library(sf)
library(ggtext)
library(showtext)

rents <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

# unique neighborhoods with only one bedrooms among units without lat/lon
nhoods.rents <- rents %>%
  filter( (is.na(lat) | is.na(lon)) & beds == 1) %>%
  mutate(location = ifelse(nhood != city, paste(nhood, ", ", city, ", ", county, " county", sep = ""),
                           paste(city, ", ", county, " county", sep = "")
                           )
  ) %>%
  select(-lat, -lon, -address)

nhoods.locations <- tibble(
  location = unique(nhoods.rents$location)
)

# geocode nhood centroids
nhoods.locations <- nhoods.locations %>%
  mutate_geocode(location = location, output = "latlona")

nhoods.rents <- left_join(nhoods.rents, nhoods.locations, by = "location")

nhoods.rents <- filter(nhoods.rents, !is.na(lat) & nhood != "south bay")

# median price of one bedrooms in each neighhorhood, each year
complete.locations <- rents %>%
  filter(beds == 1 & !is.na(lat) & !is.na(lon)) %>%
  mutate(location = NA)

# join all locations together
all.locations <- rbind(complete.locations, nhoods.rents)

# crs retrived from: spatialreference.org
all.locations <- st_as_sf(all.locations, coords = c("lon", "lat"), 
                          crs = "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

# make a grid of nhoods
limits <- st_bbox(all.locations)

grid_fine <- st_sf(st_make_grid(all.locations, # smaller area
                                square = FALSE,
                                n = c(40,40),
                                what = "polygons")) %>%
  st_filter(all.locations)

colnames(grid_fine) <- "geometry"
st_geometry(grid_fine) <- "geometry"

grid_fine <- grid_fine %>%
  mutate(id = seq(1, length(grid_fine$geometry), by=1))

grid_fine$id <- 1:nrow(grid_fine)

all.locations <- st_join(all.locations, grid_fine)

all.locations <- all.locations %>%
  group_by(id, year) %>%
  summarise(median.price = median(price),
            mean.price = mean(price),
            n.tot = n()) %>%
  ungroup() %>%
  st_drop_geometry()
 
# join with grid 
rent_grid <- left_join(grid_fine, all.locations)

# latest 5 years
rent_grid <- subset(rent_grid, year <= 2018 & year >= 2015)

# nice font
font_add_google("Patua One")
showtext_auto(enable = TRUE)

sf_basemap <- get_map(c(left = -123.16, bottom = 36.70, 
                        right = -121.50, top = 38.96),
                      source = "osm")

ggmap(sf_basemap) +
  geom_sf(data = rent_grid, aes(fill = median.price), 
          inherit.aes = FALSE, size = 0.1) +
  facet_wrap(.~year) +
  theme_minimal() +
  scale_fill_stepsn(name = "",
                   colors = rev(MetBrewer::met.brewer("OKeeffe1")),
                   labels = function(x) scales::dollar(x/1000, suffix = "k")) +
  labs(x = "", y = "",
       title = "Median prices of one-bedroom rentals in the Bay Area (2013-2018)",
       caption = "Visualization by @jmliddie | #tidytuesday week 27 | Data: Pennington, Kate (2018). Bay Area Craigslist Rental Housing Posts (2000-2018). Basemap from OpenStreetMap.") +
  theme(
        axis.text = element_blank(),
        # plot.margin = margin(10, 10, 10, 10),
        legend.key.height = unit(0.6, 'cm'),
        legend.key.width= unit(0.1, 'cm'),
        legend.direction = "vertical",
        legend.position = "left",
        legend.justification = "left",
        legend.text = element_text(size = 20, family = "Patua One"),
        plot.title = element_text(size = 40, hjust = 0, family = "Patua One"),
        strip.text = element_text(size = 30, family = "Patua One"),
        plot.caption = element_text(size = 16, hjust = 0, family = "Patua One")
        )

ggsave("2022/2022 - Week 16/sf_rents.png", dpi = 600, width = 4.2, height = 4,
       bg = "white")        
