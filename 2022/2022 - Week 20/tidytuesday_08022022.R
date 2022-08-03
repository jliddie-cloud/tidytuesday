################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 08-02-2022
################################################################################

library(tidyverse)
library(usmap)
library(sf)
library(showtext)
library(ggimage)
library(ggmap)
library(ggtext)

frogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv')

# select first sightings by frequency ID
unique_frogs <- frogs %>%
  mutate(SurveyDate = as.POSIXct(SurveyDate, format = "%m/%d/%Y")) %>%
  group_by(Frequency) %>%
  arrange(SurveyDate) %>%
  slice(1) %>%
  ungroup()
  
# frog images
img <- paste("2022/2022 - Week 20/frog.png")

unique_frogs$img <- paste(img)

# sf dataset
frogs_sf <- st_as_sf(unique_frogs, coords = c("UTME_83", "UTMN_83"), crs = "+proj=utm +zone=10")
frogs_sf <- st_transform(frogs_sf, crs = "+proj=longlat +datum=WGS84")

# reformat non-sf data
unique_frogs <- as.data.frame(frogs_sf)
unique_frogs$geometry <- str_replace_all(as.character(unique_frogs$geometry), "c?[//(//)]", "")

unique_frogs <- separate(unique_frogs, col = geometry, into = c("lat", "long"), sep = ",")
unique_frogs <- unique_frogs %>%
  mutate(across(c(lat, long), ~as.numeric(.x)))

basemap <- get_map(c(left = min(unique_frogs$lat) - 0.02, bottom = min(unique_frogs$long) - 0.01,
                     right =  max(unique_frogs$lat) + 0.02, top = max(unique_frogs$long) + 0.01),
                     source = "google", maptype = "hybrid", zoom = 14)

# nice fonts and colors
font_add_google("DM Serif Display")
showtext_auto(enable = TRUE)
colors <- MetBrewer::met.brewer(name = "Isfahan1")
colors <- c(colors[c(2, 6)], MetBrewer::met.brewer(name = "Isfahan2")[5])
  

ggmap(basemap) +
  ggimage::geom_image(data = unique_frogs, aes(image = img, x = lat, y = long, color = Type), size = 0.05, by = "height") +
  scale_color_manual(values = colors) +
  labs(title = "Oregon spotted frogs at the Crane Prairie Reservoir",
       x = "", y = "",
       subtitle = "First sightings in <span style = 'color: #845d29;'>marshes/ponds</span>, <span style = 'color: #178f92;'>the reservoir</span>, or <span style = 'color: #4063a3;'>streams/canals</span>",
       caption = "Visualization by @jmliddie | #tidytuesday week 31 | Data: USGS | Basemap from GoogleMaps") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        legend.position = "none",
        plot.subtitle = element_markdown(size = 60, lineheight = 0.1, family = "DM Serif Display"),
        plot.title = element_markdown(size = 80, lineheight = 0.1, family = "DM Serif Display"),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        plot.caption = element_text(color = "black", size = 35, hjust = 0, margin=margin(0.1), family = "DM Serif Display"),
        panel.border = element_blank(),
        panel.background = element_blank())
  
ggsave("2022/2022 - Week 20/frog_map.png", dpi = 600, width = 5, height = 5,
       bg = "antiquewhite")   
