################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 06-20-2023
################################################################################

library(tidyverse)
library(ggtext)
library(showtext)
library(sf)
library(ggmap)
library(gganimate)

ufo_sightings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')
places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/places.csv')
img <- paste("2023/2023 - Week 4/images/", list.files("2023/2023 - Week 4/images/"), sep ="")
  
# massachusetts only
MA_sightings <- ufo_sightings %>%
  filter(state == "MA" & country_code == "US" & reported_date_time >= as.POSIXct("2018-01-01"))

# get lat/long info
MA_sightings <- left_join(MA_sightings, places)

# specific shapes to focus on
shapes_to_plot <- c("chevron", "cigar", "circle", "cone", "cross", 
                    "cylinder", "diamond", "disk", "egg", "oval",
                    "rectangle", "sphere", "star", "teardrop", "triangle")

MA_sightings <- MA_sightings %>%
  filter(shape %in% shapes_to_plot) %>%
  mutate(image = paste("2023/2023 - Week 4/images/", shape, ".png", sep = ""),
         year_day_month = as.POSIXct(paste(year(reported_date_time), "-", month(reported_date_time), "-", "01", sep = ""),
                                     format = "%Y-%m-%d"),
         year_month = case_when(month(reported_date_time) < 10 ~
           paste("0", month(reported_date_time), "-", year(reported_date_time), sep = ""),
           TRUE ~ paste(month(reported_date_time), year(reported_date_time), sep = "-"))
  )

basemap <- get_map(c(left = min(MA_sightings$longitude) - 0.15, bottom = min(MA_sightings$latitude) - 0.1,
                     right = max(MA_sightings$longitude) + 0.15, top = max(MA_sightings$latitude) + 0.1),
                   source = "stamen", maptype = "toner-2011", zoom = 8)

# nice font
font_add_google("Teko")
showtext_auto(enable = TRUE)

anim.UFO <- ggmap(basemap) +
  ggimage::geom_image(data = MA_sightings, 
                      aes(image = image, x = longitude, y = latitude), 
                      color = "green4", size = 0.05, by = "height") +
  labs(title = "Massachusetts UFO sightings and their shapes in the last five years: {paste(month(current_frame), '/', year(current_frame), sep = '')}",
       caption = "Visualization by @jahredwithanh | #tidytuesday week 25 | Data: National UFO Reporting Center") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        plot.title = element_text(size = 20, hjust = 0.5,
                                  margin = margin(10, 10, 10, 10), color = "white", family = "Teko"),
        plot.caption = element_text(size = 12, hjust = 0,
                                    margin = margin(5, 5, 5, 5), color = "white", family = "Teko")
  ) +
  transition_manual(year_day_month)
  
# save plot
animate(anim.UFO, nframes = 400, fps = 15, width = 7, height = 5, units = "in",
        res = 200,
        renderer = gifski_renderer("2023/2023 - Week 4/ufo.gif"))


