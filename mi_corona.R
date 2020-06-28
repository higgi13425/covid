library(tidyverse)
library(rvest)
library(xml2)
library(leaflet)
library(maps)
library(tidycensus)
library(sf)
library(covid19us)
library(coronavirus)
library(gganimate)
library(ggrepel)
library(plotly)
library(lubridate)
library(glue)
library(slider)

options(tigris_use_cache = TRUE)

# coronavirus data ----
# scraping from state website - new data between 2-3 PM EDT each day
mi_corona <- read_html("https://www.michigan.gov/coronavirus/0,9753,7-406-98163-520743--,00.html")

#clean data to a table
data <- mi_corona %>%
  html_nodes('table') %>%
  html_table()

# clean up corona data ----
data[1] %>%
  as.data.frame() %>%
  filter( County != "Out of State") %>%
  filter( County != "Other*") %>%
  filter( County != "Unknown") %>%
  filter( County != "MDOC*") %>%
  filter( County != "FCI**") %>%
  filter( County != "Totals") %>%
  purrr::set_names("county", "cases", "deaths") %>%
  as_tibble() %>%
  mutate(cases = as.numeric(cases),
         deaths = as.numeric(deaths)) ->
  mi_corona2

# Detroit City and Wayne county have separate health departments
# for goofy historical reasons - will combine into Wayne County
mi_corona2 %>%
  filter(county == "Detroit City" |
           county == "Wayne") %>%
  summarize(cases = sum(cases),
            deaths = sum(deaths)) %>%
  mutate (county = "Wayne") %>%
  select(county, cases, deaths) ->
  wayne

mi_corona2 %>%
  filter(county != "Detroit City" &
           county != "Wayne") %>%
  bind_rows(wayne) %>%
  arrange(county) %>%
  print(n=Inf) ->
  mi_corona3



# testing data ----

test_data <- read_html("https://www.michigan.gov/coronavirus/0,9753,7-406-98163_98173_99225---,00.html")

mi_testing_list <- test_data %>%
  html_nodes('table') %>%
  html_table()

mi_testing <- mi_testing_list[[1]] %>%
  as_tibble() %>%
  janitor::clean_names()

mi_testing %>%
  mutate(ppt = parse_number(percent_positive_tests)) %>%
  select(date, ppt) %>%
  mutate(mov_avg_pct_pos = slide_dbl(ppt, mean, .before = 7),
         Date = mdy(date)) %>% print(n=Inf) %>%
  ggplot(aes(x = Date, y = mov_avg_pct_pos)) +
  geom_line(color = "red") +
  labs(y = "Percent Positive (7-day moving average)",
       x = "Date",
       title = "Percent Positive SARS-CoV-2 Tests in Michigan") +
  theme_bw(base_size = 14) +
  ylim(0,100) +
  theme(legend.position = "none") +
  annotate("text", x = ymd("2020-03-15"), y = 68, label = "66.7 on Mar10") +
  annotate("text", x = ymd("2020-03-23"), y = 20, label = "21.6 on Mar23") +
  annotate("text", x = ymd("2020-04-07"), y = 41, label = "39.2 on Apr7") +
  annotate("text", x = ymd("2020-04-28"), y = 8, label = "10.4 on May4") +
  annotate("rect", xmin = ymd("2020-03-24"), xmax = ymd("2020-04-03"), ymin = 25, ymax =35, alpha =0.2, fill = "blue" ) +
  annotate("text", x = ymd("2020-03-29"), y = 30, label = "Testing\nCapacity\nExpanded") +
  annotate("rect", xmin = ymd("2020-03-10"), xmax = ymd("2020-04-17"), ymin = 0, ymax =6, alpha =0.2, fill = "blue" ) +
  annotate("text", x = ymd("2020-03-29"), y = 3, label = "Testing Increased from 400 to 2,700 per day")

ggsave("figures/testing.png", device = "png")



# tidycensus to get county geometries
census_api_key(CENSUS_API_KEY)
michigan <- get_acs(state = "MI", geography = "county",
                    variables = "B19013_001", geometry = TRUE)

# trim off " County, Michigan" from county names
michigan2 <- michigan %>%
  mutate(county = str_sub(NAME,
                          start = 1L,
                          end = str_locate(michigan$NAME, " County, Michigan")[,1])) %>%
  mutate(county = str_trim(county))

michigan2 %>%
  select(county, geometry) %>%
  left_join(mi_corona3) %>%
  mutate(cases = replace_na(cases, 0.1)) %>%
  ggplot(aes(fill = cases)) +
  geom_sf(color = "black") +
  #coord_sf(crs = 26911) + # cool global projection if desired
  scale_fill_gradientn(colours = rev(heat.colors(7)),
                       trans = "log10",
                       name = "COVID-19\nCases") +
  geom_sf_text(aes(label = round(cases)), col="black") +
  theme_void() +
  theme(legend.position = c(0.35, 0.3)) +
  labs(title = glue("Michigan COVID-19 Cases by County on {Sys.Date()}"))



# Growth Rate -----
get_states_daily(state = "MI") %>%
  mutate(death = replace_na(death, 0.1))  ->
mi_daily

mi_daily %>%
  ggplot(aes(x=date, y = positive)) +
  scale_y_log10() +
  geom_line(col='red1', size = 1.5) +
  geom_point(size = 2) +
  #geom_text(aes(label = positive), nudge_x = -0.5, nudge_y = 0.09, size=4) +
  annotate("text", x = lubridate::ymd("2020-04-01"), y = 10000,
           label = "Cases", size = 5) +
  geom_line(aes(y = round(death)), color = "red3", size = 1.5) +
  geom_point(aes(y= round(death)), size = 2) +
  #geom_text(aes(label = round(death), x= date, y = round(death)), size = 4, nudge_x = -0.5, nudge_y = 0.09) +
  annotate("text", x = lubridate::ymd("2020-04-06"), y = 300,
           label = "Deaths", size = 5) +
  labs(y = "", x = "Date",
       title = "The Logarithmic Growth of COVID-19 in Michigan",
       subtitle = "Spring 2020",
       caption = "Graphic by P.D.R. Higgins - Data from the COVID Tracking Project API\nVia the covid19us package by Amanda Dobbyn") +
  theme_linedraw(base_size = 13)



# Growth by state -----
# pull in covid date from nytimes
covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

state_select<- "Michigan"
covid %>%
  group_by(state, date) %>%
  summarize(cases = sum(cases)) %>%
  filter(state == state_select) %>%
  ggplot(aes(date, cases)) +
  geom_line(col = "red") +
  geom_point() +
  labs(title = glue("COVID19 Cases in {state_select}"),
       subtitle = "Is the Effect of May 23 pool party starting now?") +
  theme_linedraw()





# Growth rate by county ----
# pull in covid date from nytimes
covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")


# filter covid to state_select counties
covid %>%
  mutate(county=tolower(county),
         state=tolower(state)) %>%
  filter(state == "michigan") %>%
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
plot <- covid_state %>%
  filter(county %in% top10counties_cases) %>%
  mutate(county = factor(county, levels = top10counties_cases)) %>%
  ggplot(aes(x=date, y = cases, color = county)) +
  #scale_y_log10() +
  geom_line() +
  geom_point(size = 1.5) +
  geom_text_repel(data = top10counties_cases_final,
                  aes(label = cases), nudge_x = 1) +
  scale_color_discrete(name = "County",
            labels = str_to_title(top10counties_cases)) +
  labs(y = "Cumulative Cases (log10)", x = "Date",
       title = "The Flattening of COVID-19 Spread in Michigan Counties",
       subtitle = "Which Counties are Flattening the Curve since the March 24 Shutdown?",
       caption = "Graphic by P.D.R. Higgins\nData from the NYTimes API") +
  theme_linedraw(base_size = 13) +
  theme(legend.position = c(0.09, 0.65))
plot


# animation settings
anim <- plot +
  transition_reveal(date) +
  ease_aes('cubic-in-out') +
  labs(subtitle = glue("{covid_state$date} Cases of COVID-19 Case by County"))

# render animation with an end pause
animate(anim, renderer = gifski_renderer(),
        end_pause = 22)


# save animation as a named gif in figures folder
# can move this into function if desired.
anim_save(glue("figures/county_cases_{Sys.Date()}.gif"))





## daily cases by county ----
# pull in covid date from nytimes
covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

state_select <- "michigan"

# filter covid to state_select counties
covid %>%
  mutate(county=tolower(county),
         state=tolower(state)) %>%
  filter(state == state_select) %>%
  filter(!is.na(fips))->
  covid_state

# set up end date, start date, number of days of data
end_date <- max(covid_state$date)
start_date <- min(covid_state$date)
days = as.numeric(1+ end_date - start_date)

# daily case count for each county
covid_state %>%
  arrange(county) %>%
  mutate(new_cases = cases-lag(cases)) %>%
  mutate(new_cases = case_when(new_cases<0 ~ cases,
                               is.na(new_cases) ~ cases,
         TRUE ~ new_cases)) ->
covid_daily

# make a dataframe of map data by county, adjust varnames
state_df <- map_data("county", region = state_select) %>%
  rename(state = region) %>%
  rename(county = subregion)

# make a dataframe of map data by county, adjust varnames
library(ggmap)
state_df %>%
  right_join(covid_daily) %>%
  filter(!is.na(lat))->
state_daily_geom

# set up a base (gray counties) map
state_base <- ggplot(state_df,
          mapping = aes(x=long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")

# begin with a static plot
plot <- state_base +
  theme_nothing() +
  geom_polygon(data = state_daily_geom,
               aes(fill = new_cases, group = county)) +
  geom_polygon(color = "black", fill = NA) +  # get the state border back on top
  scale_fill_gradientn(colours = rev(heat.colors(7)),
                       trans = "log10",
                       name = "COVID-19\nDaily Cases") +
  labs(caption="Graphic Animation by PDR Higgins\nData: The New York Times") +
  theme(text=element_text(size=16),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  theme(legend.position = "bottom")
plot

# animation settings
anim <- plot +
  transition_time(date) +
  ease_aes('cubic-in-out') +
    ggtitle("COVID-19 Daily Cases in {str_to_title(state_select)}", subtitle="By County: {frame_time}")

# render animation with an end pause
animate(anim, renderer = gifski_renderer(),
        end_pause = 18)

anim_save("figures/mi_counties_may10.gif")



# curves of top 10 states ----
top10states <- get_states_current() %>% arrange(desc(positive)) %>% slice(1:10) %>% pull(state)

get_states_daily() %>%
  select(date, state, positive, death) %>%
  filter(state %in% c(top10states)) %>%
  mutate(state = factor(state, levels = top10states)) %>%
  ggplot(aes(x=date, y = positive, col = state)) +
  geom_point( size =2) +
  geom_line(size = 1.5) +
  scale_y_log10() +
  theme_linedraw(base_size = 14) +
  theme(legend.position = c(0.85, 0.3)) +
  labs(title = "Logarithmic Growth of COVID-19 Cases in Top 10 US States", x = "Date", y = "Cases",
       caption = "Data from the COVID Tracking Project API\nVia the covid19us package by Amanda Dobbyn")

# consider adding animation
# as in https://github.com/thomasp85/gganimate/wiki/Temperature-time-series
library(covid19us)
library(gganimate)

top10states <- get_states_current() %>% arrange(desc(positive)) %>% slice(1:10) %>% pull(state)

end_date <- get_states_daily()[1,1] %>% pull()

get_states_daily() %>%
  replace_na(list(positive = 0)) %>%
  select(date, state, positive, death) %>%
  filter(state %in% c(top10states)) %>%
  mutate(state = factor(state, levels = top10states)) %>%
  ggplot(aes(x=date, y = positive, color = state)) +
  geom_line() +
  #geom_segment(aes(xend = end_date, yend = positive), linetype = 2, colour = 'grey') +
  geom_point() +
  #geom_text(aes(x = end_date + 0.1, y = positive,
               # label = state), hjust = 0) +
  scale_y_log10() +
  scale_color_viridis_d() +
  theme_linedraw(base_size = 14) +
  theme(legend.position = c(0.15, 0.6)) +
  labs(title = "Logarithmic Growth of COVID-19 Cases in\nTop 10 US States",
       x = "Date", y = "Cases",
       caption = "Data from the COVID Tracking Project API\nVia the covid19us package by Amanda Dobbyn") +
  transition_reveal(date)

anim_save(paste0("figures/","top10states_cases","_", end_date, ".gif"))

## version for deaths ----

top10states_d <- get_states_current() %>% arrange(desc(death)) %>% slice(1:10) %>% pull(state)

end_date <- get_states_daily()[1,1] %>% pull()

end_date_set <- get_states_daily() %>%
  filter(date == end_date) %>%
  filter(state %in% top10states_d)

get_states_daily() %>%
  filter(date > ymd("2020-Mar-08")) %>%
  replace_na(list(death = 0)) %>%
  select(date, state, positive, death) %>%
  filter(state %in% c(top10states_d)) %>%
  mutate(state = factor(state, levels = top10states_d)) %>%
  ggplot(aes(x=date, y = death, color = state)) +
  geom_line() +
  #geom_segment(aes(xend = end_date, yend = positive), linetype = 2, colour = 'grey') +
  geom_point() +
  coord_cartesian(clip = 'off') +
  geom_text(data = end_date_set,
            aes(x = date+0.5 , y = death,
                label = state), hjust = 0) +
  #scale_y_log10() +
  scale_color_viridis_d(option = 'magma') +
  theme_linedraw(base_size = 14) +
  theme(legend.position = c(0.15,0.6)) +
  labs(title = "Rapid Growth of COVID-19 Deaths in 10 US States",
       subtitle = "NY Leading",
       x = "Date", y = "Cases",
       caption = "Data from the COVID Tracking Project API\nVia the covid19us package by Amanda Dobbyn") +
  transition_reveal(date)

anim_save(paste0("figures/","top10states_deaths","_", end_date, ".gif"))

## 3D with rayshader?? ----
library(rgl)
library(rgdal)
library(rayshader)
library(rayrender)
library(tidyverse)
library(sf)

mi_map <- michigan2 %>%
  select(county, geometry) %>%
  left_join(mi_corona3) %>%
  ggplot(aes(fill = cases)) +
  geom_sf(color = "black") +
  scale_fill_gradientn(colours = rev(heat.colors(7)),
                       name = "COVID-19\nCases") +
  theme_void()

plot_gg(mi_map, multicore = TRUE, width = 4, height = 7)
Sys.sleep(0.2)
render_depth(focallength = 100, focus = 0.72)

