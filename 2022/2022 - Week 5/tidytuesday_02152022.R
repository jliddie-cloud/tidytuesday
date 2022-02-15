################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week 4
################################################################################
# ipackages
library(tidyverse)
library(showtext)

# set file loc as WD
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# data for challenge 5
dat <- readr::read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge05/data.csv")

dat <- rbind(dat, c(1863, 0, 100))

dat$for.plot <- ifelse(dat$Free / 3 > 1, 1, dat$Free / 3)

font_add_google("Teko")
showtext_auto()

ggplot(dat, aes(Year, for.plot)) +
  geom_area(fill = "#dc143c", color = "#e0d5c8") +
  geom_text(aes(Year, -0.05,
                label = ifelse(Year != 1863, paste0(Free, "%"), NA)),
                size = 4, color = "black") +
  geom_text(aes(Year, 1.05,
                label = ifelse(Year != 1863, paste0(Year), NA)), 
                size = 4, color = "black") +
  geom_text(aes(x = 1785, y = 0), 
            label = "PERCENT\nOF\nFREE BLACKS") +
  coord_flip(xlim = c(1870, 1790), ylim = c(1, 0), clip = "off") +
  scale_x_reverse(n.breaks = 9, expand = c(0,0)) +
  scale_y_reverse(n.breaks = 9, expand = c(0,0))  + # flip to the right orientation
  labs(
    title = "SLAVES AND FREE BLACKS.",
    subtitle = "\n\n",
    caption = "RECREATION BY @JMLIDDIE | #TIDYTUESDAY WEEK 7 | #DUBOISCHALLENGE2022"
  ) + 
  theme_void(base_family = "Teko") +
  theme(
    panel.grid.major.y = element_line(color = "#e0d5c8", size = 0.25),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(hjust = 0.4, size = 25, face = "bold"),
    plot.background = element_rect(fill = "#e0d5c8"),
    plot.margin = margin(0.5, 1, 0.25, 1, unit = "cm"),
    plot.caption = element_text(hjust=0, color = "black", 
                                lineheight = 1.4, size=12, margin=margin(t=15))
  )

# save plot
ggsave("free_map.png", width = 2.25, height = 4)

  

# freed_slaves <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/freed_slaves.csv")

# Wrangle
freed_slaves <- 
  freed_slaves %>% 
  janitor::clean_names() %>% 
  mutate(
    label_y = if_else(free == last(free), NA_real_, slave)
  ) %>% 
  fill(label_y)

# Plot
freed_slaves %>% 
  ggplot(aes(year, slave)) +
  geom_area(fill = "black") + 
  geom_text(aes(year, label_y + 2, label = paste0(free, "%")), size = 6, family = "Teko", fontface = "bold") +
  annotate("text", x = 1830, y = 95, label = "FREE \u2014 LIBRE", size = 8, colour = "black", family = "Teko", fontface = "bold") +
  annotate("text", x = 1830, y = 60, label = "SLAVES", size = 10, colour = "#e0d5c8", family = "Teko", fontface = "bold") +
  annotate("text", x = 1830, y = 55, label = "ESCLAVES", size = 10, colour = "#e0d5c8", family = "Teko", fontface = "bold") +
  scale_y_continuous(limits = c(0, 100), expand = c(0,0)) + 
  scale_x_continuous(limits = c(NA, NA), n.breaks = 9, 
                     expand = c(0, 0), position = "top") +
  coord_cartesian(clip = "off") +
  labs(
    title = "PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES.\n\nPROPORTION DES NÈGRES LIBRES ET DES ESCLAVES EN AMÈRIQUE.",
    subtitle = "\n\nDONE BY ATLANTA UNIVERSITY.\n\n\n\n\n",
    caption = "Dubois Visualization Challenge || Reproduced by Botan Ağın"
  ) + 
  theme_void(base_family = "Teko") + 
  theme(
    axis.text.x.top = element_text(vjust = 1, size = 18, face = "bold"),
    panel.grid.major.x = element_line(color = "#2b543a"),
    legend.position = "none",
    panel.background = element_rect(fill = "#3c7753"),
    plot.title = element_text(hjust = 0.4, size = 18, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.background = element_rect(fill = "#e0d5c8"),
    plot.margin = margin(0.5, 1, 0.25, 1, unit = "cm")
  )
