################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 01-10-2023
################################################################################

library(tidyverse)
library(usmap)
library(ggtext)
library(showtext)
library(maps)

feederwatch <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')

# data processing; remove geographical outliers + non-continental US, convert to a dataset with one row per individual
feederwatch <- feederwatch %>% filter(longitude != max(longitude))
feederwatch <- feederwatch %>% 
  filter(grepl("US", subnational1_code)) %>%
  filter(!grepl("^US-AK$", subnational1_code)) %>%
  filter(!grepl("^US-HI$", subnational1_code))

feederwatch_all <- feederwatch %>%
  uncount(how_many)

# now convert to sf with usmap crs, then drop SF while maintaining UTM as coordinates
feederwatch_all <- st_as_sf(feederwatch_all, coords = c("longitude", "latitude"),
                            crs = "+proj=longlat +datum=WGS84")
feederwatch_all <- st_transform(feederwatch_all, crs = usmap_crs())

feederwatch_all <- feederwatch_all %>%
        mutate(x = st_coordinates(.)[,1],
               y = st_coordinates(.)[,2])
feederwatch_all <- st_drop_geometry(feederwatch_all)

# begin plotting
font_add_google("Chelsea Market") # nice font
showtext_auto(enable = TRUE)

basemap <- usmap::plot_usmap(exclude = c("AK", "HI"))

basemap +
  geom_density2d_filled(data = feederwatch_all, aes(x = x, y = y), 
                        alpha = 0.75, bins = 10) +
  scale_fill_manual(values = c("white", MetBrewer::met.brewer("Hokusai2", n = 9))) +
  labs(title = "2021 Bird sightings in Project FeederWatch",
       caption = "Visualization by @jmliddie | #tidytuesday 2023 week 2 | Data from Project FeederWatch") +
  theme(legend.position = "none",
        plot.title = element_text(size = 100, hjust = 0.5, family = "Chelsea Market",
                                   margin = margin(t = 0.5, b = 0.25, unit = "cm")),
        plot.margin = margin(t = 0.25, b = 0.25, unit = "cm"),
        plot.caption = element_text(size = 40, hjust = 0, 
                                    margin = margin(t = 0.5, b = 0.125, unit = "cm"),
                                    family = "Chelsea Market", color = "darkgrey"))
  
# save
ggsave("2023/2023 - Week 2/bird_contour.png", dpi = 600, width = 8, height = 6,
       bg = "white")

