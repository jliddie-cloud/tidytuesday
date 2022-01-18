################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week 1
################################################################################

# library(tidytuesdayR)
library(tidyverse)
library(ggsankey) # dev version
library(MetBrewer)
library(ggtext)

# set file loc as WD
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

chocolate$cocoa_percent <- as.numeric(
  str_replace(chocolate$cocoa_percent, pattern = "%", replacement = "")
)

chocolate_sankey <- chocolate %>%
  filter(cocoa_percent > 85) %>% # filter for cocoa 85%+
  ggsankey::make_long(country_of_bean_origin, company_location)

chocolate_sankey <- chocolate_sankey %>% 
  group_by(node) %>%
  mutate(node_fill = cur_group_id())%>%
  ungroup() %>%
  mutate(x = factor(x, levels = c("country_of_bean_origin", "company_location"),
                    labels = c("Origin", "Company location")),
         next_x = factor(next_x, levels = c("country_of_bean_origin", "company_location"),
                             labels = c("Origin", "Company location"))
)

ggplot(chocolate_sankey, 
       aes(x = x, next_x = next_x, node = node, 
           next_node = next_node, fill = node_fill, label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 2, color = "white") +
  scale_fill_gradientn(colors = met.brewer("VanGogh1")) +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, face = "bold", color = "sienna4"),
        plot.caption = element_text(hjust=0, color = "darkgrey", 
                                    lineheight = 1.4, size=6, margin=margin(t=15)),
        plot.title = element_markdown(hjust=0.5, face = "bold", color = "sienna4",
                                  lineheight = 1.4, size=14, margin=margin(b=5)),
        plot.subtitle = element_markdown(hjust=.5, size=10, color = "sienna4")) +
  labs(caption = "Visualization by @jmliddie | #TidyTuesday Week 3",
       subtitle = "Origins and company locations of chocolate with over 85% cocoa",
       title = "*Dark chocolate: where does it come from & where does it go?*",
       x = NULL)

# save plot
ggsave("chocolate_sankey.png", width = 9, height = 7)

