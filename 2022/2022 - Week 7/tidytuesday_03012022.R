################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 03-01-2022
################################################################################
library(sf)
library(tidyverse)
library(ggthemes)
library(ggspatial)
library(usmap)
library(showtext)

stations <- st_read("2022/2022 - Week 7/Alternative_Fueling_Stations/Alternative_Fueling_Stations.shp")

MA_state_plane <- "+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000 +ellps=GRS80 +units=m +no_defs"

# boston nhoods and water bodies
nhoods <- st_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/3525b0ee6e6b427f9aab5d0a1d0a1a28_0.kml", quiet = TRUE)
water <- st_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/2b3c0fa13b1c468eb702a3645fcc6bcb_5.kml", quiet = TRUE)

# electric only; convert to sf dataframe
stations <- stations %>% filter(FUEL_TYPE_ == "ELEC")

MA_stations <- stations %>% 
  filter(STATE=="MA") %>%
  st_transform(crs = MA_state_plane)

nhoods <- nhoods %>% 
  st_transform(crs = st_crs(MA_stations))

water <- water %>%
  st_transform(MA_state_plane)

nhoods <- st_make_valid(nhoods)

nhood_stations <- st_intersection(MA_stations, nhoods)

nhoods <- nhoods %>%
  mutate(num_bus = lengths(st_covers(nhoods, bus_boston)))

# remove harbor islands for clarity in plotting
nhoods <- subset(nhoods, Name != "Harbor Islands")

# calculate the number and density of stations in each nhood
nhoods <- nhoods %>%
  mutate(num_stations = lengths(st_covers(nhoods, nhood_stations)),
        area = units::set_units(st_area(nhoods), km^2)) %>%
  mutate(station_dens = as.numeric(num_stations / area))

max.station.nhood <- nhoods$Name[nhoods$num_stations == max(nhoods$num_stations)]
max.density.nhood <- nhoods$Name[nhoods$station_dens == max(nhoods$station_dens)]

# bounding boxes
left_side  <- st_bbox(nhood_stations)$xmin
top_side <- st_bbox(nhood_stations)$ymax 

# all fonts
font_add_google("Josefin Sans")
showtext_auto()

ggplot(nhood_stations) +
  geom_sf(data = nhoods, color = "white", size = 0.1, fill = "black") +
  scale_fill_identity() +
  geom_sf(size = 0.25, color = "yellow") +
  geom_sf(data = water, fill = "lightblue", color = NA) +
  annotation_scale(location = "br") +
  theme_map() + 
  labs(title = "Electric charging stations in\nBoston neighorboods",
       caption = "Visualization by @jmliddie | #tidytuesday week 9 | Data from US DOT") +
  ggthemes::theme_map() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 30, 
                              face = "bold", family = "Josefin Sans",
                              lineheight = 0.3),
    plot.caption = element_text(hjust=0, color = "darkgrey", 
                                family = "Josefin Sans",
                                lineheight = 1.4, size = 15),
    plot.background = element_rect(color = "white"),
    )

# save plot
ggsave("2022/2022 - Week 7/stations_boston.png", width = 3, height = 3)
