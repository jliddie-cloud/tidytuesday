# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week 2

# load libraries + data
library(tidyverse)
library(geofacet)

colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')

states <- data.frame(state = datasets::state.name, 
                     state.abb = datasets::state.abb)

# number observations in each state, already arranged by date
colony <- colony %>%
  group_by(state) %>%
  mutate(date.order = row_number(state)) %>%
  ungroup() %>%
  left_join(states)

# calculate the fraction of maximum colonies in each state over the period
colony <- colony %>%
  group_by(state) %>%
  mutate(max_n = max(colony_n, na.rm = T)) %>%
  ungroup() %>%
  mutate(colony_p_max = colony_n / max_n)

my_grid <- us_state_grid2 %>%
  filter(name %in% colony$state)

# remove other states + total to avoid warning
colony <- subset(colony, state != "Other States" & state != "United States")

# now plot it all
ggplot(colony, aes(x = date.order, y = colony_p_max)) +
  geom_line() + 
  scale_y_continuous(labels=scales::percent) + 
  theme_minimal() +
  theme(plot.title = element_text(margin=margin(t=15), hjust=0.5, lineheight = 1.4, size=14),
        plot.caption = element_text(hjust=0, color = "grey", lineheight = 1.4, size=8),
        plot.subtitle = element_text(hjust=.5, size=10, margin=margin(b=10))) +
    facet_geo(~state, grid = my_grid, label = "name") +
    labs(x = NULL, y = NULL,
         title = "Percentage of maximum bee colonies in 45 states",
         subtitle = "Data span Q1 2015 to Q2 2021",
         caption = "Visualization by @jmliddie, TidyTuesday Week 2 ")
