################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 02-21-2023
################################################################################

library(tidyverse)
library(ggtext)
library(showtext)
library(BobRossColors)

bob <- read_csv("https://raw.githubusercontent.com/jwilber/Bob_Ross_Paintings/master/data/bob_ross_paintings.csv")

# summarize colors by season
seasons_wide <- bob %>%
  group_by(season) %>%
  summarise(across(Black_Gesso:Alizarin_Crimson, sum)) 

seasons_long <- pivot_longer(seasons_wide, cols = -1)

seasons_long <- seasons_long %>% 
  group_by(season) %>% 
  arrange(desc(value)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

seasons_long <- seasons_long %>%
  mutate(name = str_remove_all(name, "_")) %>%
  mutate(name = fct_relevel(name))

ten_seasons <- subset(seasons_long, season <= 10)
color_names <- unique(ten_seasons$name)

# retrieve hexes from a function in the bob ross package
retrieve_hexes <- function(x) {
  
  unique_bob_ross_colors$color_hex[unique_bob_ross_colors$color == x]

}

hexes_in_order <- map_chr(color_names, retrieve_hexes)

# now create plot with these colors
cols <- setNames(hexes_in_order, color_names)

# font
font_add_google("Cabin") # nice font
showtext_auto(enable = TRUE)

ggplot(ten_seasons, aes(x = season, y = rank, group = name)) +
  geom_line(aes(color = name), size = 4.5, alpha = 0.8) +
  scale_y_reverse(breaks = 1:nrow(ten_seasons)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_color_manual(values = cols) +
  labs(title = "The Joy of Painting",
       subtitle = "Colors ranked in order of frequency of use by Bob Ross in seasons 1-10",
       caption = "Visualization by @jmliddie | #tidytuesday 2023 week 8 | Data from Bob Ross Colors data package") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "lightblue1"),
        plot.margin = margin(r = 0.5, l = 0.5, t = 0.5, b = 0.5),
        plot.title = element_text(size = 150, hjust = 0.5, family = "Cabin",
                                  margin = margin(t = 0.125, b = 0.125,
                                                  unit = "cm")),
        plot.subtitle = element_text(size = 60, hjust = 0.5, family = "Cabin"),
        plot.caption = element_text(size = 48, hjust = 0.05, family = "Cabin",
                                    margin = margin(t = 0.125, b = 0.125,
                                                    unit = "cm"),
                                    color = "black")
        )

ggsave("2023/2023 - Week 3/bob_ross.png", dpi = 600, width = 8, height = 6)


