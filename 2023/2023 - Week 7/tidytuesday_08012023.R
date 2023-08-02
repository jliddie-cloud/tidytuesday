################################################################################
# author: Jahred Liddie (@jahredwithanh)
# purpose: tidytuesday, week of 08-01-2023
################################################################################

library(tidyverse)
library(ggtext)
library(showtext)
library(ggmap)
library(usmap)

dat <- tidytuesdayR::tt_load(2023, week = 31)

state_name_etymology <- dat$state_name_etymology

# state county geometries from usmap
states <- plot_usmap(regions = "states", exclude = "District of Columbia")

state_name_etymology <- state_name_etymology %>%
  mutate(origin_cat = case_when(grepl("via", language) ~ "Mixed/multiple etymologies",
                                state == "Arizona" | state == "Idaho" | state == "Maine" |
                                state == "Rhode Island" ~ "Mixed/multiple etymologies",
                                grepl("English", language) ~ "English",
                                language == "Basque" | language == "Spanish" |
                                grepl("French", language) | grepl("Latin", language) | 
                                grepl("Germanic", language) | grepl("Greek", language) |
                                grepl("Dutch", language) ~ "European (non-English)",
                                language == "Unknown" ~ "Unknown",
                                TRUE ~ "Native American or Hawaiian")
         )

state_name_etymology <- state_name_etymology %>%
  group_by(state) %>%
  slice(1)

states$data <- left_join(states$data, state_name_etymology, by = c("full" = "state"))

# nice font
font_add_google("Poppins")
showtext_auto(enable = TRUE)

states +
  geom_polygon(aes(x = x, y = y, group = group, fill = origin_cat), color = "black", size = 0.2) +
  scale_fill_manual(values = MetBrewer::met.brewer("Klimt")) +
  labs(title = "The many etymologies of the U.S.",
       caption = "Visualization by @jahredwithanh | #tidytuesday week 31") +
  theme(panel.background = element_rect(color = "black", fill = "black"),
        plot.background = element_rect(color = "black", fill = "black"),
        legend.key.height = unit(0.3,'cm'),
        legend.key.width = unit(0.2, 'cm'),
        legend.background = element_rect(fill = "black"),
        legend.key = element_rect(fill = "black", color = "black"),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.justification = "center",
        legend.text = element_text(color = "white", size = 20, family = "Poppins"),
        legend.title = element_blank(),
        plot.title = element_text(size = 50, hjust = 0.5, family = "Poppins", color = "white", face = "bold"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
        plot.caption = element_text(size = 15, hjust = 0, family = "Poppins", 
                                    color = "darkgrey", margin = margin(t = 0.25, unit = "cm")),
  )

ggsave("2023/2023 - Week 7/etymology_map.png", dpi = 600, width = 3.5, height = 3,
       bg = "black")        

