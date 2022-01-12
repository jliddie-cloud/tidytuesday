################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week 1
################################################################################

library(tidyverse)
library(lubridate)
library(cowplot)
library(ggtext)

# set file loc as WD
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
years <- c(2020:2022)

suffolk.covid <- data.frame()

for (i in 1:length(years)){

  # covid-19 case data from the NYTimes: https://github.com/nytimes/covid-19-data
covid.year <- readr::read_csv(
  paste('https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-counties-',
        years[i], ".csv", sep = "")
  ) %>%
  filter(county == "Suffolk" & state == "Massachusetts")

  suffolk.covid <- rbind(suffolk.covid, covid.year)
}

# wastewater data from MWRA / Biobot analytics
mwra <- read.csv("https://raw.githubusercontent.com/biobotanalytics/covid19-wastewater-data/master/wastewater_by_county.csv") %>%
  filter(name == "Suffolk" & state == "MA")

mwra$sampling_week <- as.Date(mwra$sampling_week)

### merge data
# indicators for week intervals
suffolk.covid <- suffolk.covid %>%
  filter(date >= min(mwra$sampling_week)) %>%
  mutate(since_first_day = as.numeric(date - min(date)),
         week_interval = since_first_day %/% 7)

# summarize data by week
suffolk.covid.week <- suffolk.covid %>%
  group_by(week_interval) %>%
  summarise(beg_week = min(date),
            cases = sum(cases),
            cases_avg = sum(cases_avg),
            deaths = sum(deaths),
            deaths_avg = sum(deaths_avg),
            count_days = n()
            )

all.dat <- left_join(mwra, suffolk.covid.week, 
                     by = c("sampling_week" = "beg_week"))

all.dat$lag_WW <- lag(x = all.dat$normalized_concentration_rolling_average, 
                      n = 1)
### plotting
# for scaling second axes
coef.g1 <- mean(all.dat$normalized_concentration_rolling_average) /
  mean(suffolk.covid$cases_avg)

coef.g2 <- mean(all.dat$normalized_concentration_rolling_average) / 
  mean(all.dat$cases)

# Spearman's rank correlation
corr <- cor.test(all.dat$normalized_concentration_rolling_average, 
                 all.dat$cases, method = "spearman")$estimate

# 7-day avg cases and wastewater
g1 <-
  ggplot() +
  geom_point(
    data = suffolk.covid,
    aes(y = cases_avg, x = date),
    size = 0.5, color = "darkblue") +
  geom_line(
    data = all.dat,
    aes(y = normalized_concentration_rolling_average/coef.g1,
        x = sampling_week),
    size = 2, color = "darkgreen", linetype = "dotted"
  ) +
  scale_y_continuous(
    name = "Cases (7-day avg.)",
    sec.axis = sec_axis(trans = ~.*coef.g1, 
                        name = "Concentration [copies/mL]")
  ) + 
  theme_light() +
  theme(
    axis.title.y = element_text(color = "darkblue", size=10),
    axis.title.y.right = element_text(color = "darkgreen", size=10),
    plot.subtitle = element_markdown(hjust=.5, size=14, margin=margin(b=10)),
    plot.title = element_text(hjust=0.5, 
                              lineheight = 1.4, size=16, margin=margin(b=10)),
    axis.text.x = element_blank()
  ) +
  labs(title = "COVID-19 cases and wastewater concentrations",
       subtitle = "*7-day avg. cases in Suffolk County, MA*",
       x = "")

g2 <- 
  ggplot(all.dat, aes(x = sampling_week)) +
    geom_line(
      aes(y = cases),
          size = 1, color = "darkblue") +
    geom_line(
      aes(y = normalized_concentration_rolling_average/coef.g2),
      size = 1, color = "darkgreen", linetype = "dotted") +
    scale_y_continuous(
      name = "Cases (weekly total)",
      sec.axis = sec_axis(trans = ~.*coef.g2, 
                          name = "Concentration [copies/mL]")
      ) + 
    theme_light() +
    theme(
      axis.title.y = element_text(color = "darkblue", size=10),
      axis.title.y.right = element_text(color = "darkgreen", size=10),
      plot.subtitle = element_markdown(hjust=.5, size=14, margin=margin(b=10)),
      plot.caption = element_text(hjust=0, color = "darkgrey", 
                                  lineheight = 1.4, size=8, margin=margin(t=15)),
    ) +
  labs(subtitle = 
         paste("*Weekly total cases in Suffolk, MA (r = ",
               round(corr, 2), ")*", sep = ""),
       x = "Date",
       caption = "Estimate using Spearman's rank correlation shown above. COVID-19 data from NYT and wastewater data from BioBot / MWRA | Visualization by @jmliddie | #TidyTuesday Week 1")

cowplot::plot_grid(g1, g2, nrow = 2, ncol = 1, align = "v")

# save plot
ggsave("COVID_MWRA.png", width = 6, height = 8)

