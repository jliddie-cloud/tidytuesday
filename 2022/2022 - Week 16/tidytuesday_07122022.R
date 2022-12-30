################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 07-12-2022
################################################################################
library(tidyverse)
library(ggmap)
library(sf)
library(ggtext)
library(showtext)

flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')

greece.airports <- tibble(APT_ICAO = unique(flights$APT_ICAO[flights$STATE_NAME=="Greece"]),
                          location = paste(APT_ICAO, ", Greece", sep = ""))

greece.airports <- greece.airports %>%
  mutate_geocode(location = location, output = "latlona")


greece_basemap <- get_map(c(left = 19.6, bottom = 34.7, 
                            right = 28.4, top = 41.9),
                            source = "stamen", maptype = "toner")

greece.flights <- flights %>%
  filter(APT_ICAO %in% greece.airports$APT_ICAO & (YEAR <= 2021 & YEAR >= 2018)) %>%
  group_by(APT_ICAO, APT_NAME, YEAR) %>%
  summarise(FLT_TOT_1 = sum(FLT_TOT_1)) %>%
  ungroup()

greece.airports <- left_join(greece.airports, greece.flights)

# nice font
font_add_google("Cormorant SC")
showtext_auto(enable = TRUE)

greece.breaks <- c(1700, 5000, 15000, 45000, 110000, 220000)

ggmap(greece_basemap) +
  geom_point(data = greece.airports, aes(x = lon, y = lat, size = FLT_TOT_1, color = FLT_TOT_1),
             inherit.aes = FALSE, alpha = 0.8) +
  ggrepel::geom_label_repel(data = greece.airports, aes(label = APT_NAME),
                            label.padding = 0.1, seed = 10, size = 14,
                            family = "Cormorant SC") +
  theme_minimal() +
  scale_size_continuous(name = "Arrivals + departures", 
                        trans = "sqrt",
                        breaks = greece.breaks, 
                        labels = function(x) {paste(round(x/1000, 1), "k", sep = "")}) +
  scale_color_gradientn(name = "Arrivals + departures", 
                        colors = MetBrewer::met.brewer(name = "Greek"),
                        breaks = greece.breaks,
                        trans  = "sqrt",
                        labels = function(x) {paste(round(x/1000, 1), "k", sep = "")}) +
  guides(color = guide_legend()) +
  facet_wrap(~YEAR) +
  labs(x = "", y = "",
       title = "Flights in and out of Greek airports (2018-2021)",
       caption = "Visualization by @jmliddie | #tidytuesday week 28 | Data: Eurocontrol | Basemap from Stamen") +
  theme(
        axis.text = element_blank(),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        legend.direction = "vertical",
        legend.position = "left",
        legend.justification = "left",
        legend.margin = margin(t = 0.05, unit='cm'),
        legend.text = element_text(size = 40, family = "Cormorant SC", colour = "white"),
        legend.title = element_text(size = 45, family = "Cormorant SC", colour = "white"),
        plot.title = element_text(size = 65, hjust = 0, family = "Cormorant SC",colour = "white", face = "bold"),
        strip.text = element_text(size = 50, family = "Cormorant SC", colour = "white", face = "bold"),
        plot.caption = element_text(size = 40, hjust = 0, family = "Cormorant SC", margin=margin(t=0.5),
                                    colour = "white")
        )

ggsave("2022/2022 - Week 17/greece_map.png", dpi = 500, width = 7, height = 7,
       bg = "black")        
