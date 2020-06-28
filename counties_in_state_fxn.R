# Which Counties are Flattening the Fastest?

library(tidyverse)
library(lubridate)
library(ggrepel)
library(glue)

county_growth <- function(state_name) {

# # Growth rate by county ----
# pull in covid date from nytimes
covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# filter covid to state_select counties
covid %>%
  mutate(county=tolower(county),
         state=tolower(state)) %>%
  filter(state == state_name) %>%
  filter(!is.na(fips))->
  covid_state

# set up end date, start date, number of days of data
end_date <- max(covid_state$date)
start_date <- min(covid_state$date)
days = as.numeric(1+ end_date - start_date)

# select Top 10 Counties by case or deaths
top10counties_cases <- covid_state %>%
  filter(date == end_date) %>%
  arrange(desc(cases)) %>%
  slice(1:10) %>%
  pull(county)

top10counties_deaths <- covid_state %>%
  filter(date == end_date) %>%
  arrange(desc(deaths)) %>%
  slice(1:10) %>%
  pull(county)

top10counties_cases_final <- covid_state %>%
  filter(date == end_date) %>%
  arrange(desc(cases)) %>%
  slice(1:10)

# Plot
covid_state %>%
  filter(county %in% top10counties_cases) %>%
  filter(date > ymd("2020-Mar-03")) %>%
  mutate(county = factor(county, levels = top10counties_cases)) %>%
  ggplot(aes(x=date, y = cases, color = county)) +
  scale_y_log10() +
  geom_line() +
  geom_point(size = 1.5) +
  geom_text_repel(data = top10counties_cases_final, aes(label = cases), nudge_x = 1) +
  scale_color_discrete(name = "County",
                       labels = str_to_title(top10counties_cases)) +
  labs(y = "", x = "Date",
       title = glue("The Logarithmic Growth of COVID-19 in {str_to_title(state_name)} Counties"),
       subtitle = "Starting to Bend the Curve?",
       caption = "Graphic by P.D.R. Higgins\nData from the NYTimes API") +
  theme_linedraw(base_size = 13) +
  theme(legend.position = c(0.15, 0.75)) +
  theme(legend.box.background = element_rect(color="black"),
    legend.box.margin = margin(6, 6, 6, 6))
}

