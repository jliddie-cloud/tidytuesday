################################################################################
# author: Jahred Liddie (@jahredwithanh)
# purpose: tidytuesday, week of 01-07-2025
################################################################################
library(tidyverse)
library(ggtext)
library(showtext)

# set file loc as WD
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# credit to ajstarks for making these data available online:
ga_data <- read_csv('https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/refs/heads/master/plate19/data.csv')

dates <- unique(ga_data$Date)
dates <- -sort(desc(dates))

ga_data <- ga_data %>%
  mutate(Date = as.factor(Date),
         label = ifelse(Date == 1874, "338,769",
                        ifelse(Date == 1899, "1,062,223", NA)))

font_add_google("Teko")
showtext_auto()

ga_data$Date <- fct_relevel(ga_data$Date, paste(dates))

ggplot(ga_data, aes(x = Land, y = Date)) +
  geom_bar(stat = "identity", fill = "#DA4A59", width = 0.6, color = "black", linewidth = 0.2) +
  geom_text(aes(label = label), vjust = 0.5, hjust = 0.5,
            position = position_stack(vjust = 0.5),
            color = "black", fontface = "bold",
            size = 5) +
  labs(
    title = "ACRES OF LAND OWNED BY NEGROES\nIN GEORGIA.",
    caption = "RECREATION BY @JAHREDWITHANH | #TIDYTUESDAY WEEK 1"
  ) + 
  theme_void(base_family = "Teko") +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "black", size = 20,
                               margin = margin(t = 0, r = -10, b = 0, l = 10)),
    panel.background = element_rect(fill = "#e0d5c8", colour = "#e0d5c8"),
    plot.title = element_text(hjust = 0.5, size = 38, lineheight = 0.5),
    plot.background = element_rect(fill = "#e0d5c8"),
    plot.caption = element_text(hjust = 0, color = "black",
                                lineheight = 1.4, size = 18, margin = margin(t = 10, b = 5))
  )

ggsave("dubois_graph2.png", width = 4, height = 5)
