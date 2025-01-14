################################################################################
# author: Jahred Liddie (@jahredwithanh)
# purpose: tidytuesday, week of 01-14-2025
################################################################################
library(tidyverse)
library(wordcloud2)
library(ggtext)
library(showtext)
library(tidytext)

# set file loc as WD
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

posit <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-14/conf2024.csv')

wordcloud2(data = posit$session_title, size=1.6)

count_posit <- posit %>%
  unnest_tokens(output = word, input = talk_title) %>%
  count(word, sort = TRUE)

count_posit <- count_posit %>%
  # filter with those beginning/ending in the stop_words 
  filter(!grepl(paste("^", stop_words$word, "$", sep = "", collapse = "|"), word)) %>%
  filter(n > 1)

posit_wordcloud <- 
  wordcloud2(data = count_posit, size = 1.2, shape = "diamond",
             backgroundColor = "black", color = "random-light", 
             fontFamily = "helvetica")

# export here, or within the viewer manually for better quality:
htmlwidgets::saveWidget(posit_wordcloud, "tmp.html", selfcontained = F)
webshot::webshot("tmp.html", "posit_wordcloud.png", delay = 0.5, vwidth = 800, vheight = 600)
