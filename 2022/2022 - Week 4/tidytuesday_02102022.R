################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week 4
################################################################################
# ipackages
library(tidyverse)
library(showtext)
library(usmap)

# set file loc as WD
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read data
airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

# count of airmen by state
state.count <- airmen %>%
  group_by(state) %>%
  summarise(total.airmen = n()) %>% ungroup()
 
# US state map + join with state.count
state.map <- plot_usmap(regions = "states")
state.map[["data"]] <- left_join(state.map[["data"]], state.count, 
                       by = c("abbr" = "state"))

# filter HI and AK out
state.map[["data"]] <- state.map[["data"]] %>% filter(abbr != "HI" & abbr != "AK")

state.map[["data"]] <- state.map[["data"]] %>%
  mutate(total.airmen.group = case_when(total.airmen < 5 ~ "1 - 4",
                                        total.airmen < 25 ~ "5 - 24",
                                        total.airmen < 45 ~ "25 - 44",
                                        total.airmen < 65 ~ "45 - 64",
                                        total.airmen > 65 ~ "65+",
                                        is.na(total.airmen) ~ "NONE")
  )

state.map[["data"]]$total.airmen.group <- factor(state.map[["data"]]$total.airmen.group,
                                                 levels = c("NONE", "1 - 4", "5 - 24", "25 - 44",
                                                            "45 - 64", "65+"))

# Google fonts (https://fonts.google.com/)
font_add_google("Teko", "teko")
showtext_auto()

# plot
state.map +
  geom_polygon(aes(x = x, y = y, group = group, fill = total.airmen.group), 
               color = "black") +
  scale_fill_manual(name = "", 
                    values = c("grey", "#ffd700",
                               "#ffc0cb", "#dc143c",
                               "#d2b48c", "#654321")) +
  labs(title = "TUSKEGEE AIRMEN BY HOME STATE.",
       caption = "VISUALIZATION BY @JMLIDDIE | #TIDYTUESDAY WEEK 6 | #TUSKEGEEAIRMENCHALLENGE #DUBOISCHALLENGE2022") +
  theme(
      text = element_text(family="teko"),
      plot.title=element_text(size=60, hjust = 0.5),
      plot.caption = element_text(hjust=0, color = "darkgrey", 
                                  size=20),
      plot.background = element_rect(fill="#f9efe6", color=NA),
      legend.position = "bottom",
      legend.title = element_text(hjust = 0.5),
      legend.text = element_text(size=40),
      legend.background = element_rect(fill="#f9efe6", color=NA),
      legend.box.background = element_rect(fill="#f9efe6", color=NA)
      )

# save plot
ggsave("tuskegee_map.png", width = 5, height = 5)
