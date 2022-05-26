################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 05-04-2022
################################################################################
library(tidyverse)
library(lubridate)
library(ggtext)
library(showtext)

sevens <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/sevens.csv')

# let's group by year...
sevens <- sevens %>%
  mutate(year = year(date)
  )

# calculate wins and losses for the US team
wins <- sevens %>%
  filter(winner == "United States") %>%
  group_by(year) %>%
  summarise(outcome = n()) %>%
  mutate(category = "Win") %>%
  ungroup()

losses <- sevens %>%
  filter(loser == "United States") %>%
  group_by(year) %>%
  summarise(outcome = n()*-1) %>%
  mutate(category = "Loss") %>%
  ungroup()

winslosses <- rbind(wins, losses)


winslosses <- winslosses %>%
  group_by(year) %>%
  mutate(games = sum(abs(outcome)),
         perc = round(abs(outcome)/games*100, 0)) %>%
  ungroup()
        
# nice font
font_add_google("Abril Fatface")
font_add_google("Roboto")
showtext_auto(enable = TRUE)

ggplot(data = winslosses) + 
  geom_bar(aes(x = year,y = outcome,fill = category, width = 0.7), 
           stat = "identity", position = "identity") +
  geom_text(aes(x = year,y = outcome, 
                label = ifelse(category == "Win", paste(abs(outcome), " (", perc, "%)", sep = ""), 
                               paste(abs(outcome)))
                ),
            check_overlap = TRUE,
            color = "white", size = 6,
            vjust = ifelse(winslosses$outcome >= 0, -2, 3)) +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values = c("#e76254", "#376795")) +
  labs(
    title = "*The US Women's Rugby Sevens Team: <span style = 'color: #376795;'>Wins</span> and <span style = 'color: #e76254;'>Losses</span>* (1997-2022)",
    x = "", y = "",
    caption = "*Visualization by @jmliddie | #tidytuesday week 21 | Data from ScrumQueens*") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background=element_rect(fill='black', color='black'),
        panel.border = element_rect(colour = "black"),
        plot.background=element_rect(fill='black', color='black'),
        axis.text.y = element_blank(),
        plot.title = element_markdown(face = "bold", size = 40, 
                                      color = "white", family = "Abril Fatface"),
        legend.position = "none",
        plot.margin=margin(1,2,1,0,'cm'),
        plot.caption = element_markdown(color = "white", size = 15, family = "Roboto", hjust = 0),
        axis.text.x = element_text(vjust = 0, color = "white", size = 20, face = "bold")
        )

# save plot
ggsave("2022/2022 - Week 10/rugby.png", width = 10, height = 8, dpi = 200,
       bg = "#f5f5f5")

