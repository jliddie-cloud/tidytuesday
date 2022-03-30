################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 03-29-2022
################################################################################
library(tidyverse)
library(ggtext)
library(showtext)
library(MetBrewer)

sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

font_add_google("Playfair Display")
showtext_auto(enable = TRUE)

# tennis only
tennis <- sports %>% 
  filter(sports == "Tennis" & !is.na(total_exp_menwomen))

# expenditures per player and general sector name
tennis <- tennis %>% mutate(
  exp_p_player = total_exp_menwomen / (sum_partic_men + sum_partic_women),
  general_sector = case_when(grepl("Private nonprofit", sector_name) ~ "Private, non-profit",
                             grepl("Private for-profit", sector_name) ~ "Private, for-profit",
                             grepl("Public", sector_name) ~ "Public"
                             )
)

tennis <- tennis %>%
  mutate(general_sector = factor(general_sector, levels = c("Private, non-profit",
                                                            "Private, for-profit",
                                                            "Public")
                                 )
         )

# calculate medians
median_exp_p_player <- tennis %>% 
  group_by(year) %>%
  summarise(median_total = mean(exp_p_player)) %>%
  mutate(institution_name = "Overall median",
         general_sector = "Overall median")

# set colors
colors <- c("black", met.brewer("Hiroshige", 3))

ggplot(tennis) + 
  geom_line(aes(x = year, 
                y = exp_p_player, 
                group = institution_name, 
                color = general_sector),
            alpha = 0.4, size = 0.25) + 
  geom_label(
    label = paste("Max: $", round(max(tennis$exp_p_player), -4)/1000, 
                  "K", sep = ""),
    x = tennis$year[tennis$exp_p_player == max(tennis$exp_p_player)],
    y = log(max(tennis$exp_p_player) + 20000, base = 10),
    label.padding = unit(0.2, "lines"), # Rectangle size around label
    label.size = 0.2,
    size = 10,
    color = "black",
    fill = "white",
    family = "Playfair Display") +
  geom_line(data = median_exp_p_player, size = 0.5, linetype = "dashed",
            aes(x = year, y = median_total, color = general_sector)) +
  scale_color_manual(values = colors, name = "") +
  # thicker legend lines
  guides(color = guide_legend(override.aes = list(size = 1, alpha = 1))) +
  geom_label(
    data = median_exp_p_player,
    label = paste("$", round(median_exp_p_player$median_total[median_exp_p_player$year==2018], -3)/1000, "K",
                  sep = ""),
    x = 2018,
    y = log(median_exp_p_player$median_total[median_exp_p_player$year==2018] + 5000, base = 10),
    label.padding = unit(0.1, "lines"), # Rectangle size around label
    label.size = 0.1,
    color = "black",
    fill = "white",
    size = 10,
    family = "Playfair Display") +
  scale_y_log10(labels=scales::dollar_format(), expand = c(0.07, 0)) +
  labs(y = "Expenditures per athlete", x = "",
       title = "There's a wide range of expenditures per athlete in college tennis",
       subtitle = "However, yearly spending per athlete is stable overall",
       caption = "Visualization by @jmliddie | #tidytuesday week 13 | Data from Equity in Athletics Data Analysis") +
  theme_bw() +
  theme(text = element_text(family = "Playfair Display"),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 22),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 22),
        legend.position = "top",
        legend.justification = "left",
        legend.text = element_text(size = 18),
        plot.title.position = "plot",
        plot.margin = margin(t = 20,  
                             r = 50,  
                             b = 20,  
                             l = 20),
        plot.caption =  element_text(hjust=0, lineheight = 1.4, 
                                     size = 18, color = "grey"),
        plot.caption.position = "plot")

# save plot
ggsave("2022/2022 - Week 8/tennis.png", width = 8, height = 6, dpi = 200)

