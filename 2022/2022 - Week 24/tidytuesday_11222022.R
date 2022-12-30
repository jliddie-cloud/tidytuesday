################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 11-22-2022
################################################################################
library(tidyverse)
library(ggtext)
library(showtext)
library(sf)
library(ggmap)

museums <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-22/museums.csv')


# focus on art museums in london!
london_arts <- museums %>% filter(grepl("London$", `Village,_Town_or_City`) &
                             Subject_Matter == "Arts-Fine_and_decorative_arts")


london_arts <- london_arts %>%
  mutate(Area_Geodemographic_group = case_when(Area_Geodemographic_group == "Ethnically Diverse Metropolitan Living" ~
                                                 "Ethnically Diverse\nMetropolitan Living",
                                               Area_Geodemographic_group == "University Towns and Cities" ~
                                                 "University Towns\nand Cities",
                                               TRUE ~ as.character(Area_Geodemographic_group))
         ) %>%
  mutate(Area_Geodemographic_group = fct_relevel(Area_Geodemographic_group, 
                                                 "London Cosmopolitan",
                                                 "Ethnically Diverse\nMetropolitan Living",
                                                 "University Towns\nand Cities",
                                                 "Rural-Urban Fringe")
         )

# london basemap after using st_bbox
london_basemap <- get_map(c(left = -0.35, bottom = 51.42, 
                            right = 0.05, top = 51.58), zoom = 12,
                            maptype = "toner", source = "stamen")

# nice fonts
font_add_google("Courgette") # plot title
font_add_google("Prompt") # label and other text
showtext_auto(enable = TRUE)

ggmap(london_basemap) +
  geom_label_repel(data = london_arts, 
                  aes(x = Longitude, y = Latitude, label = Name_of_museum,
                      fill = Area_Geodemographic_group), 
                  color = "white", fontface = "bold", segment.color = "red",
                  family = "Prompt", max.overlaps = 100, force = 5, label.r = 0.25,
                  max.time = 10, seed = 2022, box.padding = 0.5, min.segment.length = 1,
                  size = 15, segment.curvature = -0.1, segment.angle = 20) +
  scale_fill_brewer(name = "", palette = "Set1") +
  labs(x = "", y = "", title = "Most of London's art museums are located in the city center",
       caption = "Visualization by @jmliddie | #tidytuesday week 47 | Data from the Mapping Museums project") +
  ggthemes::theme_map() +
  theme(plot.title = element_markdown(hjust = 0.5, lineheight = 2, size = 128, 
                                      family = "Courgette", margin = margin(b = 20, t = 10),
                                      face = "bold", color = "white"),
        plot.caption = element_markdown(hjust = 0, margin = margin(t = 5, b = 5), color = "white",
                                        lineheight = 1.4, size = 50, family = "Prompt"),
        legend.text = element_text(size = 50, family = "Prompt", lineheight = 0.15, color = "white"),
        legend.key.height = unit(0.1, 'cm'),
        legend.key.width = unit(0.1, 'cm'),
        legend.direction = "horizontal",
        legend.position = "top",
        legend.justification = 0.5,
        legend.spacing.x = unit(0.25, 'cm'),
        legend.background = element_blank()) +
  guides(fill = guide_legend(title = "", override.aes = aes(label = "", size = 4)))

ggsave("2022/2022 - Week 24/london_museums.png", dpi = 600, width = 10, height = 8, bg = "black")
