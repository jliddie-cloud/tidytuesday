################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 12-20-2022
################################################################################
library(tidyverse)
library(showtext)
library(ggtext)
library(ggrepel)
library(ggmap)

# load data
weather_forecasts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/weather_forecasts.csv')
cities <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/cities.csv')

weather_forecasts <- weather_forecasts %>% filter(possible_error == "none") # remove possible reporting errors
weather_forecasts$error <- abs(weather_forecasts$observed_temp - weather_forecasts$forecast_temp)

city_error <- weather_forecasts %>%
  group_by(city, state) %>%
  summarise(mean_error = mean(error, na.rm = T))

cities <- left_join(cities, city_error)

# cities <- st_as_sf(cities, coords = c("lon", "lat"), crs = 3857, remove = FALSE)

cities <- cities %>% filter(!is.na(mean_error))

cities <- cities %>%
  mutate(callout = case_when(city == "FAIRBANKS" ~ "Fairbanks, AK has the greatest forecast error",
                              TRUE ~ as.character(NA)))

# nice font
font_add_google("DM Sans")
showtext_auto(enable = TRUE)

us_basemap <- get_map(c(left = -158, bottom = 20, 
                        right = -60, top = 65),
                      maptype = "roadmap", source = "google")

ggmap(us_basemap) +
  geom_point(data = cities, 
             aes(fill = mean_error, x = lon, y = lat), 
             color = "black",
             shape = 22,
             size = 2.5,
             stroke = 0.25) +
  scale_fill_stepsn(name = "Avg. temperature\nforecast error [°F]",
                    colors = rev(colorspace::heat_hcl(4))) +
  geom_label_repel(data = cities, aes(label = callout), 
                   nudge_y = -4,
                   nudge_x = -3,
                   family = "DM Sans",
                   fontface = "bold",
                   box.padding = 0.5,
                   segment.curvature = -0.1,
                   segment.angle = 20,
                   size = 25) +
  labs(x = "", y = "", title = "Average daily temperature forecast error in 167 U.S. cities",
       subtitle = "Avg. error calculated as the mean absolute difference between<br>forecasted and observed daily temperatures",
       caption = "Visualization by @jmliddie | #tidytuesday week 51 | Data from USA National Weather Service | Basemap from Google Maps") +
  theme(plot.title = element_markdown(hjust = 0,lineheight = 2, size = 128, family = "DM Sans", margin = margin(b = 10, t = 10),
                                      face = "bold"),
        plot.subtitle = element_markdown(hjust = 0, size = 96, lineheight = 0.25, family = "DM Sans", margin = margin(t = 5, b = 5),
                                         face = "italic"),
        plot.caption = element_markdown(hjust = 0, margin = margin(t = 0), lineheight = 1.4, size = 64, family = "DM Sans"),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(size = 64, family = "DM Sans", lineheight = 0.15,
                                    margin = margin(b = -20), face = "bold"),
        legend.text = element_text(size = 64, family = "DM Sans", lineheight = 0.15,
                                   margin = margin(l = -20)),
        legend.direction = "vertical",
        legend.position = "right",
        legend.justification = "center",
        legend.key.height = unit(0.8, 'cm'),
        legend.key.width= unit(0.8, 'cm')) +
  guides(fill = guide_colourbar(ticks.colour = NA))

ggsave("2022/2022 - Week 22/forecasts.png", dpi = 600, width = 10, height = 10, bg = "white")
