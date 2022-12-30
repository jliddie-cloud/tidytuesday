################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 09-27-2022
################################################################################
library(tidyverse)
library(showtext)
library(geofacet)
library(ggalluvial)
library(ggtext)

# load data and state helper dataframe
artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-27/artists.csv')

states <- rbind(
  data.frame(state = datasets::state.name, 
             state.abb = datasets::state.abb),
  data.frame(state = c("District\nof Columbia", "Puerto Rico"),
             state.abb = c("DC", "PR"))
)

# set up data for plotting, calculate necessary new vars
artists <- artists %>%
  mutate(POC = ifelse(race == "White", "White", "People of color"))

state.summaries <- artists %>%
  group_by(state, POC) %>%
  summarise(artists_n = sum(artists_n, na.rm = T),
            workers_n = sum(all_workers_n)) %>%
  ungroup()

totals <- artists %>%
  group_by(state) %>%
  summarise(total_artists = sum(artists_n, na.rm = T),
            total_workers = sum(all_workers_n)) %>%
  ungroup()

state.summaries <- left_join(state.summaries, totals)

state.summaries$perc_artists <- state.summaries$artists_n / state.summaries$total_artists
state.summaries$perc_workers <- state.summaries$workers_n / state.summaries$total_workers
state.summaries$diff <- state.summaries$perc_workers - state.summaries$perc_artists

# finally, convert to plot format needed
state.summaries <- state.summaries %>%
  select(state, POC, perc_artists, perc_workers) %>%
  pivot_longer(cols  = c("perc_artists", "perc_workers"), names_to = "type")

state.summaries <- state.summaries %>%
  mutate(type.clean = ifelse(type == "perc_artists", "Artists", "All workers")) %>%
  mutate(type.clean = factor(type.clean, levels = c("Artists", "All workers")))

state.summaries <- left_join(state.summaries, states)

# grid of states; adding puerto rico for completeness
my_grid <- us_state_grid2

my_grid <- rbind(my_grid,
                 data.frame(row = 8, col = 12, code = "PR", name = "Puerto Rico")
                 )

# nice font
font_add_google("Kanit")
showtext_auto(enable = TRUE)

ggplot(state.summaries, aes(x = type.clean, y = value)) +
  geom_flow(aes(alluvium = POC), lty = 2, fill = "white", color = "black",
            curve_type = "linear", width = 0.5, size = 0.15) +
  geom_col(aes(fill = POC), width = 0.5, color = "black", size = 0.25) +
  scale_fill_manual(values = c("#a40062","grey"), name = "") +
  labs(x = "", y = "", title = "In every US state, <span style = 'color: #a40062;'>people of color</span> make up a lesser share of artists than all workers",
       caption = "Visualization by @jmliddie | #tidytuesday week 39 | Data from arts.gov by way of Data is Plural") +
  theme_minimal() +
  theme(plot.title = element_markdown(margin = margin(t = 10, b = 20), hjust = 0, 
                                      lineheight = 2, size = 70, family = "Kanit"),
        plot.caption = element_markdown(hjust = 0, margin = margin(t = 10), color = "grey",
                                        lineheight = 1.4, size = 35, family = "Kanit"),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 25, color = "darkgrey", family = "Kanit"),
        strip.text.x = element_text(size = 30, family = "Kanit"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 35, color = "black", family = "Kanit"),
        legend.key.size = unit(0.5, 'cm')) +
  facet_geo(~state, grid = my_grid, label = "name")

ggsave("2022/2022 - Week 23/artists.png", dpi = 600, width = 8.5, height = 6, bg = "white")
