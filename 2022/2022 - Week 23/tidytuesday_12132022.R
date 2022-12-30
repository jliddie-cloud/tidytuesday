################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 12-13-2022
################################################################################
library(tidyverse)
library(ggtext)
library(showtext)
library(ggrepel)

state_retail <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-13/state_retail.csv',  col_types = "cciciiccc")

state_retail <- state_retail %>%
  mutate(date = as.Date(paste("01", month, year, sep = "-"), format = "%d-%m-%Y"))
  

total_retail <- state_retail %>% filter(subsector == "total" & change_yoy != "S")

# format variables and text callouts
total_retail <- total_retail %>%
  mutate(change_yoy = as.numeric(change_yoy),
         type = ifelse(state_abbr == "USA", "USA", "state"),
         callout = case_when(date == "2020-01-01" & state_abbr == "USA" ~ "First COVID case reported in US",
                             date == "2021-02-01" & state_abbr == "USA" ~ "By Feb. 2021, >100 million vaccine doses had been\nadministered in the US",
                             TRUE ~ as.character(NA)))

# nice font
font_add_google("Arvo")
showtext_auto(enable = TRUE)

# final plot
ggplot(total_retail, aes(x = date, y = change_yoy,
                           group = state_abbr, 
                           color = type, size = type, alpha = type)) + 
  geom_line() +
  scale_color_manual(name = "", values = c("grey", "darkblue")) +
  scale_size_manual(name = "", values = c(0.5, 2)) +
  scale_alpha_manual(name = "", values = c(0.5, 1)) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months", expand = c(0,0),
               limits = as.Date(c("2019-01-01", "2022-08-01"))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1),
                     breaks = seq(-25, 75, 25)) +
  geom_text_repel(aes(label = callout),
    family = "Arvo", fontface = "bold", size = 18,
    direction = "y", hjust = 0, segment.size = 0.5, seed = 2022,
    segment.alpha = 1, point.padding = 3,
    segment.curvature = ifelse(total_retail$date == "2020-01-01", 0.4, -0.4), 
    segment.angle = ifelse(total_retail$date == "2020-01-01", 70, -70), 
    segment.ncp = 10, lineheight = 0.15,
    arrow = arrow(length = unit(0.015, "npc")),
    nudge_y = ifelse(total_retail$date == "2020-01-01", 20, -20)
  ) +
  labs(x = "", y = "YOY change", title = "The COVID-19 pandemic has had a profound impact on US retail sales",
       subtitle = "YOY changes in the <span style = 'color: darkblue;'>**US overall**</span> and <span style = 'color: grey;'>each US state</span>",
       caption = "Visualization by @jmliddie | #tidytuesday week 50 | Data from the United States Census Bureau") +
  ggthemes::theme_fivethirtyeight() +
  theme(plot.title = element_text(hjust = 0.5, lineheight = 2, size = 100, 
                                  family = "Arvo", margin = margin(b = 10, t = 10)),
        plot.subtitle = element_markdown(hjust = 0.5, size = 80, lineheight = 0.25,
                                     family = "Arvo", margin = margin(t = 10, b = 10)),
        plot.caption = element_text(hjust = 0, margin = margin(t = 10),
                                    lineheight = 1.4, size = 50, family = "Arvo"),
        axis.text = element_text(size = 50, family = "Arvo"),
        axis.title = element_text(size = 50, family = "Arvo"),
        plot.margin = margin(t = 10, b = 10, l = 30, r = 30),
        legend.position = "none")

ggsave("2022/2022 - Week 23/retail.png", dpi = 600, width = 10, height = 7)
  
