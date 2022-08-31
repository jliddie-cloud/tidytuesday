################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 08-30-2022
################################################################################

library(tidyverse)
library(sf)
library(showtext)
library(gganimate)

pell <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv')

# loading hexgrid from https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map
hex <- st_read("2022/2022 - Week 22/data/us_states_hexgrid.geojson")

# load some inflation data from the WB and format
  inflation <- read.csv("2022/2022 - Week 22/data/us_inflation.csv")
  inflation$date <- as.Date(inflation$date, format = "%Y-%m-%d")
  inflation$year <- lubridate::year(inflation$date)
  
  inflation$inflation_rate[inflation$year==1999] <- 0 # 1999 is the base year
  
  inflation <- inflation %>%
    mutate(denom = 1 + inflation_rate/100,
           sum_denom = cumprod(denom)) # denominator for inflation adjustment calculation (i.e., a cumulative product)

# state abbreviations
states <- tibble(name = state.name, abbr = state.abb)
states <- rbind(states,
                tibble(name = "District of Columbia", abbr = "DC"))

# create dataset for map
state_awards <- pell %>%
  group_by(STATE, YEAR) %>%
  summarise(award = sum(AWARD, na.rm = T),
            recipient = sum(RECIPIENT, na.rm = T)) %>%
  ungroup()

state_awards <- state_awards %>%
  filter(STATE %in% states$abbr)

state_awards <- left_join(state_awards, inflation %>% select(YEAR = year, sum_denom))

state_awards <- state_awards %>%
  mutate(apr = award / recipient,
         apr_adjusted = apr/sum_denom)


# configure hex grid with labels
  hex <- hex %>%
    mutate(google_name = gsub(" \\(United States\\)", "", google_name))
  
  hex <- st_transform(hex, crs = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  hex <- left_join(hex, states, by = c("google_name" = "name"))

  hex <- left_join(hex, state_awards, by = c("abbr" = "STATE"))
  
  centroids <- st_centroid(hex)
  
  centroids <-  centroids %>%
    mutate(long = unlist(map(centroids$geometry, 1)),
           lat = unlist(map(centroids$geometry, 2)))
  
  centroids <- left_join(centroids, state_awards, 
                         by = c("abbr" = "STATE"))
  
anim.pell <-
  ggplot(hex) + 
    geom_sf(aes(fill = apr_adjusted)) + 
    geom_text(data = centroids, aes(x = long, y = lat, label = abbr),
              color = "white", size = 2) +
    scale_fill_gradient(low = "white",
                        high = "darkorchid",
                        breaks = c(1000, 2000, 3000),
                        limits = c(1000, 3400),
                        labels = scales::dollar_format(),
                        name = "Inflation-adjusted award / recipient") +
    labs(title = "Pell grant award per recipient",
         subtitle = "Year: {current_frame}",
         caption = "Visualization by @jmliddie | #tidytuesday week 35 | Data from the US Department of Education") +
    ggthemes::theme_map() +
    theme(
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.justification = "center",
          legend.key.height = unit(0.5, 'cm'),
          legend.key.width= unit(1.6, 'cm'),
          legend.title = element_text(size = 8),
          plot.title = element_text(hjust = 0, size = 20, 
                                    face = "bold"),
          plot.subtitle = element_text(hjust = 0, size = 10, 
                                       lineheight = 2),
          plot.caption = element_text(hjust=0, color = "darkgrey", 
                                      lineheight = 2, size = 8)) +
    guides(fill = guide_colourbar(ticks = FALSE, title.position = "top")) +
  transition_manual(YEAR)

# save plot
animate(anim.pell, nframes = 400, fps = 30, width = 7, height = 5, units = "in",
        res = 200,
        renderer = gifski_renderer("2022/2022 - Week 22/pell.gif"))

