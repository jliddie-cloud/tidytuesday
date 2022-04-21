################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 03-29-2022
################################################################################
library(tidyverse)
library(data.table)
library(ggtext)
library(showtext)
library(MetBrewer)

times <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/times.csv')

# letter the answer starts with?
times[, first.letter := substr(answer, start = 1, stop = 1)]

# how long was the answer?
times[, answer.length := nchar(answer)]

summary.counts <- table(times[, first.letter], times[, answer.length])

summary.counts <- as.data.frame(summary.counts)

summary.counts <- summary.counts %>%
  rename(first.letter = Var1, 
         answer.length = Var2) %>%
  mutate(answer.length = as.numeric(answer.length))

# nice font
font_add_google("Alfa Slab One")
showtext_auto(enable = TRUE)

# densities of word-count combinations (<= 20 letters long)
summary.counts %>% 
  filter(answer.length <= 15 & answer.length > 2) %>%
  ggplot(aes(x = first.letter,y = answer.length, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue", name = "") +
  coord_cartesian(ylim = c(3, 15)) +
  scale_y_continuous(breaks = c(3:15)) +
  labs(x = "First letter", y = "Answer length") +
  theme_minimal() +
  theme(text = element_text(family = "Noto Serif"),
        panel.grid = element_blank(),
        legend.position = "none")
  
summary.counts %>% 
  filter(answer.length <= 15 & answer.length > 2) %>%
  ggplot(aes(x = first.letter,y = answer.length, fill = Freq)) +
  geom_raster() +
  scale_fill_gradientn(colors = met.brewer("OKeeffe2"), name = "") +
  coord_cartesian(ylim = c(3, 15)) +
  scale_y_continuous(breaks = c(3:15)) +
  labs(x = "", y = "", title = "A NEW YORK TIMES CROSSWORD HEATMAP",
       subtitle = "Frequencies of the first letter & answer length from over 100,000 answers",
       caption = "Visualization by @jmliddie | #tidytuesday week 16 | Data from Cryptic Crossword Clues") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 24, hjust = 0.5, family = "Alfa Slab One", face= "bold"),
        plot.subtitle = element_markdown(size = 18, hjust = 0.5, family = "Alfa Slab One"),
        axis.text = element_text(size = 20),
        axis.title = element_blank(),
        plot.title.position = "plot",
        plot.caption =  element_text(hjust=0, lineheight = 1.4, family = "Alfa Slab One",
                                     size = 15, color = "grey"),
        plot.caption.position = "plot")

# save plot
ggsave("2022/2022 - Week 9/crossword.png", width = 6, height = 6, dpi = 200,
       bg = "#f5f5f5")

