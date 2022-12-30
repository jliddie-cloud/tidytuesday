################################################################################
# author: Jahred Liddie (@jmliddie)
# purpose: tidytuesday, week of 06-07-2022
################################################################################
library(tidyverse)
library(ggpubr)
library(ggtext)
library(showtext)

companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/static_list.csv')
# corp_by_politicians <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/corp_by_politician.csv')

# rename some vars
companies <- companies %>%
  rename(company = "Company",
         pride = "Pride?",
         HRC = "HRC Business Pledge",
         amount = "Amount Contributed Across States") %>%
  mutate(log_amount = log(amount))

# remove grand total
companies <- companies %>% filter(company != "Grand Total")

# add new var
companies <- companies %>%
  mutate(pride_HRC = case_when(pride == TRUE & HRC == TRUE ~ "Pride sponsor & HRC business pledge",
                               pride == TRUE & HRC == FALSE ~ "Pride sponsor only",
                               pride == FALSE & HRC == TRUE ~ "HRC business pledge only",
                               pride == FALSE & HRC == FALSE ~ "Neither"),
         either = ifelse(pride == TRUE | HRC == TRUE, "HRC pledge / pride sponsor", "Neither")
         )

# nice font
font_add_google("Permanent Marker")
font_add_google("Roboto")
showtext_auto(enable = TRUE)

my_comparisons <- list(c("HRC pledge / pride sponsor", "Neither"))

t_test.pride <- t.test(companies$log_amount ~ companies$either)

ggviolin(companies, x = "either", y = "amount", fill = "either",
         palette = c("grey", "darkgrey"),
         add.params = list(fill = "white")) +
  geom_jitter(aes(color = log_amount), size = 2) +
  scale_color_gradientn(name = "", colors = rainbow(8)) +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", size = 6,
                     label.y = 7) + # Add significance levels
  scale_y_log10(labels = scales::dollar_format()) +
  labs(x = "", y = "Donations to anti-LGBTQ politicians",
       # title = "Corporate <span style = 'color: #376795;'>ra</span><span style = 'color: #376795;'>inb<\span><span style = 'color: #376795;'>ow<\span>washing",
       title = "Corporate <span style = 'color: red;'>ra</span><span style = 'color: blue;'>inb</span><span style = 'color: darkgreen;'>ow</span> washing",
       subtitle = "On average, many 'pro'-pride businesses donate just as much as others to anti-LGBTQ politics",
       caption = paste("p-value from two-sided t-test: ", round(t_test.pride$p.value, 2), " | Visualization by @jmliddie | #tidytuesday week 21 | Data from Data for Progess", sep = "")) +
  theme(legend.position = "none",
        plot.title = element_markdown(size = 35, 
                                      family = "Permanent Marker"),
        plot.subtitle = element_text(size = 25, 
                                      family = "Permanent Marker"),
        axis.text = element_text(size = 20, 
                                 family = "Permanent Marker"),
        axis.text.y = element_text(angle = 45),
        axis.title = element_markdown(size = 25, 
                                      family = "Permanent Marker"),
        plot.caption = element_text(color = "black", size = 15, family = "Roboto", hjust = 0),
        )


# save plot
ggsave("2022/2022 - Week 12/pride.png", width = 10, height = 8, dpi = 200,
       bg = "#f5f5f5")

