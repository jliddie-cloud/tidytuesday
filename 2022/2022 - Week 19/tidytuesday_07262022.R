################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 07-26-2022
################################################################################

library(tidyverse)
library(tidycensus)
library(usmap)
library(sf)
library(showtext)

vars <- tidycensus::load_variables(year = 2020)

decennial <-  get_decennial(
  geography = "county",
  variables = "P1_001N",
  year = 2020
) %>%
  rename(tot_pop = value)

# filter our PR
decennial <- decennial %>% filter(!grepl("Puerto Rico", NAME))

# cumulative pop and % of total
decennial <- decennial %>%
  arrange(desc(tot_pop)) %>%
  mutate(sum_pop = cumsum(tot_pop),
         frac_pop = sum_pop / sum(tot_pop))

decennial <- decennial %>%
  mutate(pop_group = ifelse(frac_pop <= 0.5, "Top 50%", "Bottom 50%"))


# generate FIPS codes and create GEOIDs
fipscodes <- tidycensus::fips_codes

# Create GEOID
fipscodes$GEOID <- paste0(fipscodes$state_code, fipscodes$county_code, sep = "") %>%
  as.integer()

# Create state_county col
fipscodes$state_county <- paste(fipscodes$state, fipscodes$county, sep = '_') %>%
  tolower()

# Clean up for matching
fipscodes$state_county <- str_replace(fipscodes$state_county, " county", "")

# remove everything but GEOID and county name
fipscodes[,c(2, 4:5)] <- NULL

fipscodes$GEOID <- as.character(fipscodes$GEOID)
fipscodes$GEOID <- sprintf("%05s", fipscodes$GEOID)

decennial <- left_join(decennial, fipscodes)

# state county geometries from usmap
state.counties <- plot_usmap(regions = "counties")
state.counties$data$county <- str_replace(string = state.counties$data$county,
                                          pattern = " County", 
                                          replacement = "")
state.counties$data$state_county <- paste(tolower(state.counties$data$abbr), 
                                          tolower(state.counties$data$county), 
                                          sep = "_")

# new name for alaskan census areas
state.counties$data$state_county <- ifelse(
  state.counties$data$state_county == "ak_valdez-cordova census area",
  "ak_chugach census area",
  state.counties$data$state_county
)

state.counties$data <- left_join(state.counties$data, 
                                 decennial %>% select(state_county, tot_pop, frac_pop, pop_group)
)

# nice font
font_add_google("Secular One")
showtext_auto(enable = TRUE)

state.counties +
  geom_polygon(aes(x = x, y = y, group = group, fill = pop_group), color = "black",
               size = 0.1) +
  scale_fill_manual(name = "", 
                    values = c("Top 50%" = "green", 
                               "Bottom 50%" = "white")) +
  labs(title = paste("Half of the US population lives in these", 
                     sum(decennial$pop_group=="Top 50%"), "counties"),
       caption = "Visualization by @jmliddie | #tidytuesday week 30 | Data from the 2010-2020 US Census") +
  theme(panel.background = element_rect(color = "white", fill = "white"),
        legend.position = "none",
        plot.title =  element_text(size = 40, hjust = 0.5, family = "Secular One"),
        plot.margin=unit(c(0.5,0.5,0.25,0.5),"cm"),
        plot.caption = element_text(size = 18, hjust = 0, 
                                    family = "Secular One", color = "darkgrey",
                                    margin=margin(t = 0.25, unit = "cm")),
  )

ggsave("2022/2022 - Week 19/us_population.png", dpi = 600, width = 4, height = 3,
       bg = "white")   
