################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 06-28-2022
################################################################################
library(tidyverse)
library(ggtext)
library(showtext)
library(ggrepel)

paygap <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv')

# hourly pay gaps by year and size
paygap.summary <- paygap %>%
  filter(employer_size != "Not Provided") %>%
  group_by(lubridate::year(date_submitted), employer_size) %>%
  summarise(diff_mean_hourly_percent = mean(diff_mean_hourly_percent, na.rm = T),
            diff_mean_bonus_percent = mean(diff_mean_bonus_percent, na.rm = T),
            n = n()) %>%
  rename("year" = "lubridate::year(date_submitted)") %>%
  ungroup()

# removing 2017 for plotting, as few companies reported stats in that year; may be outlying
paygap.summary <- filter(paygap.summary, year != 2017)

paygap.summary <- paygap.summary %>%
  mutate(name_label = ifelse(year==2022, employer_size, NA), # label for plot
         employer_size = fct_relevel(employer_size, c("Less than 250", "250 to 499", 
                                                      "500 to 999", "1000 to 4999",
                                                      "5000 to 19,999", "20,000 or more"))
         )

# nice font
font_add_google("Arvo")
showtext_auto(enable = TRUE)

ggplot(paygap.summary, aes(x = year, y = diff_mean_hourly_percent, 
                           group = employer_size, color = employer_size)) +
  # these act as gridlines
  geom_vline(
    xintercept = seq(2018, 2022, by = 1),
    color = "grey91", 
    size = .6
  ) +
  geom_point() +
  geom_line() +
  geom_text_repel(
    aes(label = name_label),
    family = "Arvo",
    fontface = "bold",
    size = 10,
    direction = "y",
    xlim = c(2022.5, NA),
    hjust = 0,
    segment.size = 0.7,
    segment.alpha = 0.5,
    segment.linetype = "dotted",
    box.padding = 0.4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  ) +
  scale_color_manual(values = c("goldenrod", MetBrewer::met.brewer("Tam", n = 6)[2:6])) +
  coord_cartesian(
    clip = "off",
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(10, 16, by = 1),
    labels = glue::glue("{format(seq(10, 16, by = 1), nsmall = 0)}%")
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(2018, 2023.5), 
    breaks = seq(2018, 2022, by = 1)
  ) +
  labs(x = "", y = "",
       title = "Employers of all sizes have a gender pay gap in the UK",
       subtitle = "Average percent differences in hourly wages of men & women where positive = higher wages for men",
       caption = "Visualization by @jmliddie | #tidytuesday week 26 | Data: gender-pay-gap.service.gov.uk") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.margin = margin(10, 10, 10, 10),
        plot.background = element_rect(fill = "grey98", color = "grey98"),
        panel.background = element_rect(fill = "grey98", color = "grey98"),
        plot.title = element_text(hjust = 0.5, size = 34, family = "Arvo", face = "bold"),
        plot.subtitle  = element_text(
          family = "Arvo",
          size = 22,
          lineheight = 1.5, 
          hjust = 0.5),
        axis.text = element_text(size = 18, family = "Arvo"),
        plot.caption = element_text(size = 15, family = "Arvo", hjust = 0))

ggsave("2022/2022 - Week 14/paygap.png", dpi = 400, width = 5, height = 3.5)        
