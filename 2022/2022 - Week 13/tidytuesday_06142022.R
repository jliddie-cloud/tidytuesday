################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 06-14-2022
################################################################################
library(tidyverse)
library(gganimate)
library(lubridate)
library(ggtext)
library(showtext)

drought_fips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv')

# summarizing california data
california <- subset(drought_fips, State == "CA")

ca_average <- california %>%
  group_by(date) %>%
  summarise(DSCI = mean(DSCI))

# around the peak
ca_average$max <- ifelse(ca_average$DSCI >= max(ca_average$DSCI) - 0.05*max(ca_average$DSCI), 
                         "Peak of 2011-2017 drought",
                         "")

# adding a pause for the gif near the peak
ca_average <- ca_average %>%
  mutate(show_time = case_when(max == "Peak of 2011-2017 drought" ~ 25,
                               TRUE  ~  5),
         reveal_time = as.integer(cumsum(show_time))) %>%
  ungroup() %>%
  mutate(ID = row_number())

# seasons
ca_average <- ca_average %>%
  mutate(season = case_when(month(date) >= 3 & month(date) < 6 ~ "Spring",
                            month(date) >= 6 & month(date) < 9 ~ "Summer",
                            month(date) >= 9 & month(date) < 12 ~ "Fall",
                            TRUE ~ "Winter"))

# nice font
font_add_google("PT Serif")
showtext_auto(enable = TRUE)

anim.ts <- ggplot(ca_average, aes(x = date, y=DSCI)) +
              geom_line() +
              geom_point(size = 3) + 
              geom_text(aes(label = max, x = date), y=500, size=5, 
                        family = "PT Serif") +
              geom_hline(aes(yintercept = DSCI), linetype = 2, color = "grey") + 
              ylim(0, 510) +
              labs(title = "The extent of drought in California: 2000-2022",
                   subtitle = "Zero means none of the area is in drought & 500 means all of the area is in exceptional drought",
                   y = "Drought score [0-500 scale]", x = "",
                   caption = "Visualization by @jmliddie | #tidytuesday week 23 | Data from the National Integrated Drought Information System") +
              transition_reveal(along = reveal_time) +
              theme_minimal() +
              theme(plot.title = element_markdown(size = 20, family = "PT Serif",
                                                  hjust = 0),
                    plot.subtitle = element_text(size = 10, hjust = 0,
                                                  family = "PT Serif"),
                    axis.text = element_text(size = 10),
                    axis.title = element_markdown(size = 10, 
                                                   family = "PT Serif"),
                    plot.caption = element_text(size = 8, family = "PT Serif", hjust = 0))


# save plot
animate(anim.ts, nframes = 400, fps = 20, width = 7, height = 5, units = "in",
        res = 150,
        renderer = gifski_renderer("2022/2022 - Week 13/drought.gif"))
