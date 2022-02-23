################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week 5
################################################################################

library(tidyverse)
library(gganimate)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(showtext)
library(MetBrewer)

freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

freedom <- subset(freedom, Region_Name == "Europe")

# Europe map
world <- ne_countries(scale = "medium", returnclass = "sf")
europe <- world[which(world$continent == "Europe"),]


# view any big differences
setdiff(europe$sovereignt, freedom$country)

freedom <- freedom %>%
  mutate(country = case_when(grepl("United Kingdom", country) ~ "United Kingdom",
                             grepl("Czech", country) ~ "Czech Republic",
                             grepl("Russia", country) ~ "Russia",
                             grepl("Moldova", country) ~ "Moldova",
                             grepl("Macedonia", country) ~ "Macedonia",
                             grepl("Serbia", country) ~ "Republic of Serbia",
                             TRUE ~ as.character(country)
                             )
         )

europe <- left_join(europe, freedom,
                     by = c("sovereignt" = "country")
                     )

europe <- subset(europe, !is.na(year))

# all fonts
font_add_google("Raleway")
showtext_auto()

# animated plot
anim.map <- ggplot(europe, aes(fill = CL)) +
                    geom_sf(color = "white") +
                    scale_fill_gradient(low = met.brewer("Hiroshige")[1], 
                                        high = met.brewer("Hiroshige")[10],
                                        breaks = c(1, 3.5, 6),
                                        labels = c("High", "Middle","Low"),
                                        trans = "reverse",
                                        name = "Civil liberty\nscore") +
                    coord_sf(xlim = c(-20,50), ylim = c(33,73), expand = TRUE) +
                    labs(title = "Civil liberty in Europe",
                        subtitle = "Year: {current_frame}",
                        caption = "Visualization by @jmliddie | #tidytuesday week 8\nData from Freedom House and the United Nations by way of Arthur Cheib") +
                    ggthemes::theme_map() +
                    theme(
                      axis.text.x = element_blank(),
                      axis.text.y = element_blank(),
                      legend.position = "right",
                      plot.title = element_text(hjust = 0.5, size = 20, 
                                                face = "bold", family = "Raleway",
                                                lineheight = 1.4,),
                      plot.subtitle = element_text(hjust = 0, size = 10, 
                                                   family = "Raleway",
                                                   lineheight = 1.4,),
                      plot.caption = element_text(hjust=0, color = "darkgrey", 
                                                  family = "Raleway",
                                                  lineheight = 1.4, size = 8)) +
                    transition_manual(year)

animate(anim.map, nframes = 30, fps = 2, width = 5, height = 5, units = "in",
        res = 150,
        renderer = gifski_renderer("2022/2022 - Week 6/europe.gif"))

