################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 07-12-2022
################################################################################
library(tidyverse)
library(ggtext)
library(showtext)
library(ggstream)

tech <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')

countries <- countrycode::codelist


US.energy <- tech %>%
  filter(iso3c == "USA" & grepl("^Electricity from", label))

US.energy <- US.energy %>%
  mutate(plot.name = case_when(grepl("coal", label) ~ "Coal",
                               grepl("gas", label) ~ "Natural gas",
                               grepl("hydro", label) ~ "Hydropower",
                               grepl("nuclear", label) ~ "Nuclear",
                               grepl("oil", label) ~ "Oil",
                               grepl("other", label) ~ "Other renewables",
                               grepl("solar", label) ~ "Solar",
                               grepl("wind", label) ~ "Wind",
                               TRUE ~ as.character(NA))
         )

US.energy$plot.name <- fct_relevel(US.energy$plot.name,
                                   "Wind", "Solar", "Hydropower","Other renewables",
                                   "Coal", "Natural gas", "Oil",
                                   "Nuclear")

# nice font
font_add_google("Koulen")
showtext_auto(enable = TRUE)


ggplot(US.energy, aes(x = year, y = value, fill = plot.name)) +
  geom_stream(color = "black", size = 0.5) +
  scale_fill_manual(values = c(rev(MetBrewer::met.brewer(name = "VanGogh3", n = 4)),
                               rev(MetBrewer::met.brewer(name = "OKeeffe2", n = 3)),
                               "cadetblue")
                    ) +
  labs(caption = "Visualization by @jmliddie | #tidytuesday week 29 | Data: data.nber.org",
       title = "Electricity production in the US (1980-2020)") +
  theme_minimal() +
  theme(legend.position="top",
        legend.margin = margin(t = 0.5, unit='cm'),
        axis.text.y=element_blank(),
        axis.title=element_blank(),
        axis.text.x = element_text(size = 40, face = "bold", family = "Koulen", color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 40, family = "Koulen"),
        legend.spacing.x = unit(0.4, 'cm'),
        legend.spacing.y = unit(0.2, 'cm'),
        panel.grid.major.y =element_blank(),
        panel.grid.minor.y =element_blank(), 
        plot.title =  element_text(size = 70, hjust = 0.5, family = "Koulen"),
        plot.margin=unit(c(0.5,0.5,0.25,0.5),"cm"),
        plot.caption = element_text(size = 30, hjust = 0, family = "Koulen", margin=margin(t = 0.25, unit = "cm")),
        plot.background = element_rect(fill = "lightgrey")
  )


ggsave("2022/2022 - Week 18/electricity.png", dpi = 500, width = 7, height = 5)        
