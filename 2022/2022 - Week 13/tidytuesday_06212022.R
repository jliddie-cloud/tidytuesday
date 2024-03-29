################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 06-21-2022
################################################################################
library(tidyverse)
library(geosphere)
library(maps)
library(grDevices)
library(ggmap)

slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')

city_totals <- slave_routes %>%
  group_by(port_origin, place_of_purchase, port_arrival) %>%
  summarise(total_slaves = sum(n_slaves_arrived, na.rm = T)) %>%
  ungroup()

# 20 routes with most slaves (after filtering)
city_totals <- city_totals %>%
  arrange(desc(total_slaves)) %>%
  filter(port_origin != port_arrival & # filter out port city == port arrival
         !is.na(port_origin) & !is.na(port_arrival) & !is.na(place_of_purchase)
         )

most_cities <- city_totals[1:100,]

# clean up some names before geocoding
most_cities <- most_cities %>%
  mutate(clean_origin = case_when(port_origin == "Southeast Brazil, port unspecified" ~ "Southeast Region, Brazil",
                                  grepl(", port unspecified", port_origin) ~ 
                                    str_replace(string = port_origin, pattern = ", port unspecified", replacement = ""),
                                  TRUE ~ port_origin),
         # note: this simplifies many of the locations
         clean_middle = case_when(grepl("Africa., port unspecified", place_of_purchase) ~ "West Africa",
                                  place_of_purchase == "Loango" ~ "Republic of the Congo",
                                  grepl(", port unspecified", place_of_purchase) ~ 
                                    str_replace(string = place_of_purchase, pattern = ", port unspecified", replacement = ""),
                                  place_of_purchase == "Bonny" ~ "Rivers State, Nigeria",
                                  place_of_purchase == "West Central Africa and St. Helena, port unspecified" ~ "Saint Helena",
                                  place_of_purchase == "Windward + Ivory + Gold + Benin" ~ "Benin",
                                  place_of_purchase == "Cape Lahou" ~ "Grand-Lahou",
                                  TRUE ~ place_of_purchase),
         clean_arrival = case_when(grepl(", port unspecified", port_arrival) ~ 
                                     str_replace(string = port_arrival, pattern = ", port unspecified", replacement = ""),
                                   port_arrival == "Kingston" ~ "Kingston, Jamaica",
                                   port_arrival == "Demerara" ~ "Demerara, Guiana",
                                   port_arrival == "Freetown" ~ "Freetown, Sierra Leone",
                                   port_arrival == "Charleston" ~ "Charleston, South Carolina",
                                   port_arrival == "Cayes (Les)" ~ "Les Cayes, Haiti",
                                   port_arrival == "Campos" ~ "Campos, Brazil",
                                   TRUE ~ port_arrival)
         )

# manual fix for two areas
most_cities$clean_arrival[most_cities$clean_arrival=="St. Vincent"] <- "st vincent and the grenadines"
most_cities$clean_middle[most_cities$clean_middle=="Windward Coast"] <- "West Africa"
most_cities$clean_middle[most_cities$clean_middle=="Gold Coast"] <- "Gulf of Guinea"


# geocode cities/locations
most_cities <- most_cities %>%
  mutate(across(clean_origin:clean_arrival,
                ~geocode(location = .x, output = "latlon"),
                .names = "geo_{.col}")
         )

# get connections
origin_middle <- gcIntermediate(most_cities$geo_clean_origin, most_cities$geo_clean_middle, breakAtDateLine = T)
middle_arrival <- gcIntermediate(most_cities$geo_clean_middle, most_cities$geo_clean_arrival)

# now plot
png(file = "2022/2022 - Week 14/trade_triangle.png", width = 6, height = 5, units = "in", res = 300)

  # slightly zoomed map
  map("world", fill=T, col="white", bg = "grey7",
      ylim = c(-50, 60),
      xlim = c(-100, 40),
      )
  
  # origins and destinations
  points(most_cities$geo_clean_origin, pch=16, cex=0.5, col="orange")
  points(most_cities$geo_clean_middle, pch=16, cex=0.5, col="green")
  points(most_cities$geo_clean_arrival, pch=16, cex=0.5, col="blue")
  
  # arcs connecting them
  for (i in seq_along(origin_middle)){
    lines(origin_middle[[i]], lwd = 0.5, col = "turquoise1")  
  }
  
  for (j in seq_along(middle_arrival)){
    lines(middle_arrival[[j]], lwd = 0.5, col = "turquoise1")  
  }
  
  title(main = "Largest routes of the\ntransatlantic slave trade",
        col.main = "white", 
        sub = "Visualization by @jmliddie | #tidytuesday week 25 | Data from slavevoyages.org",
        col.sub = "white", cex.sub = 0.5, line = 0.3)
  
  legend(x="bottomright", 
         legend=c("Origin ports", "Middle ports", "Arrival ports"), 
         col=c("orange","green","blue"), bg = "grey7", text.col = "white",
         pch=c(16, 16), cex = 0.75, border = "grey7")

dev.off()

