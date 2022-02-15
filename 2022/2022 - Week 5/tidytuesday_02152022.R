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
ggsave("dubois_graph.png", width = 2.25, height = 4)

  