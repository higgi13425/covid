
library(tidyverse)
library(shadowtext)
library(gganimate)
library(gifski)
library(glue)

## data from
## https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series

options(scipen = 20)
covid <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")


## barplot code ----
# find total cases for China
covid %>%
  select(2, ncol(.)) %>%
  purrr::set_names('country', 'cases') %>%
  filter(country== 'China') %>%
  summarise(sum = sum(cases)) ->
china_num

# find total cases for US
covid %>%
  select(2, ncol(.)) %>%
  purrr::set_names('country', 'cases') %>%
  filter(country== 'US') %>%
  summarise(sum = sum(cases)) ->
  us_num

# fix total cases for Japan - add cruise ship
covid %>%
  select(2, ncol(.)) %>%
  purrr::set_names('country', 'cases') %>%
  filter(country== 'Japan' | country == 'Cruise Ship') %>%
  summarise(sum = sum(cases)) ->
  japan_num

# fix south korea name
covid$`Country/Region`[covid$`Country/Region` == 'Korea, South'] <- 'South Korea'

# cases by country
covid %>%
  select(2, ncol(.)) %>%
  purrr::set_names('country', 'cases') %>%
  distinct(country, .keep_all = TRUE) %>%
  mutate(country = as.factor(country)) %>%
  arrange(desc(cases)) ->
covid2

#fix china total
covid2$cases[covid2$country == 'China'] <- china_num$sum

#fix us total
covid2$cases[covid2$country == 'US'] <- us_num$sum

#fix japan total
covid2$cases[covid2$country == 'Japan'] <- japan_num$sum

covid2 %>%
  arrange(desc(cases)) %>%
  slice(1:10) %>%
  ggplot(mapping = aes(x= fct_reorder(country,cases),
                       y = cases, fill = country)) +
  geom_col() + #V1
  geom_text(aes(x= fct_reorder(country,cases),
                y = cases*0.5, size =16,
                label = prettyNum(cases, big.mark = ","))) +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(legend.position = 'none') +
  labs(x = "", y = "Cases of Coronavirus",
       title = glue("Total COVID19 Cases on {Sys.Date()}"),
       caption = "Data from the JHU Center for Systems Science and Engineering (CSSE) GIS") +
  scale_y_log10()

## heatmap code ----
library(hrbrthemes)
library(plotly)

covid <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

covid %>%
  filter(`3/14/20` >500) %>%
  gather(date, cases, 5:ncol(.)) %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(country = `Country/Region`, date) %>%
  summarise(cases = sum(cases)) %>%
  ggplot(aes(y = country,
             x = date,
             fill = log(cases))) +
  geom_tile() +
  theme_ipsum(base_size = 14) +
  theme(legend.position = 'none') +
  labs(x='', y = '',
       title = "Heatmap of Cumulative COVID19 Cases\nBy Country in 2020") +
  scale_fill_viridis_c() ->
heatmap
heatmap

## lollipop plot ----
library(ggalt)
covid %>%
  filter(`4/4/20` > 10000) %>%
  gather(date, cases, 5:ncol(.)) %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(country = `Country/Region`, date) %>%
  summarise(cases = sum(cases)) %>%
  filter(date == Sys.Date()) %>%
  ggplot(aes(x = fct_reorder(country, cases),
             y = cases)) +
  geom_lollipop(point.colour = 'blue', point.size = 3) +
  scale_y_log10() +
  coord_flip() +
  theme_linedraw(base_size = 14) +
  labs(title = "Total Cases", subtitle = glue("On {Sys.Date()}"),
       x = '', y = "Cases of Coronavirus")

## scatter plot ----
## # to fix with SKor name, Japan total, China total
covid %>%
  filter(`3/18/20` >100) %>%
  select(1:2, ncol(.)) %>%
  purrr::set_names('state', 'country', 'cases')->
cases_max

deaths <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv') %>%
  select(1:2, ncol(.)) %>%
  purrr::set_names('state', 'country', 'deaths')

left_join(cases_max, deaths, by = c('state', 'country')) %>%
  filter(country != 'China') %>%
  arrange(desc(cases)) %>%
  distinct(country, .keep_all = TRUE) %>%
  slice(1:20) %>%
  filter(deaths >0) %>%
  ggplot(aes(x=deaths, y = cases)) +
  geom_point() +
  geom_text(aes(label = country), nudge_x = 0.045,
            nudge_y =0.03) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal() +
  geom_smooth(method = 'lm')

## animate code ----
covid %>%
  gather(date, cases, 5:ncol(.)) %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(country = `Country/Region`, date) %>%
  summarise(cases = sum(cases)) %>%
  filter(country != "Others" & country != "Mainland China") %>%
  bind_rows(
    tibble(country = "Republic of Korea", date = as.Date("2020-03-11"), cases = 7755)
  ) %>%
  group_by(country) %>%
  mutate(days_since_100 = as.numeric(date-min(date[cases >= 100]))) %>%
  ungroup() %>%
  filter(is.finite(days_since_100)) %>%
  group_by(country) %>%
  mutate(new_cases = cases-cases[days_since_100 == 0]) %>%
  filter(sum(cases >= 100) >= 5) %>%
  filter(cases >= 100) %>%
  bind_rows(
    tibble(country = "33% daily rise", days_since_100 = 0:18) %>%
      mutate(cases = 100*1.33^days_since_100)
  ) %>%
  ungroup() %>%
  mutate(
    country = country %>% str_replace_all("( SAR)|( \\(.+)|(Republic of )", "")
  ) %>%
  # filter(days_since_100 <= 10) %>%
  ggplot(aes(days_since_100, cases, col = country)) +
  geom_hline(yintercept = 100) +
  geom_vline(xintercept = 0) +
  geom_line(size = 0.8) +
  geom_point(pch = 21, size = 1) +
  scale_y_log10(expand = expand_scale(add = c(0,0.1)), breaks=c(100, 200, 500, 1000, 2000, 5000, 10000,100000)) +
  scale_y_continuous(expand = expand_scale(add = c(0,100))) +
  scale_x_continuous(expand = expand_scale(add = c(0,1))) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(3,15,3,3,"mm")
  ) +
  coord_cartesian(clip = "off") +
  scale_colour_manual(values = c("United Kingdom" = "#ce3140", "US" = "#EB5E8D", "Italy" = "black",
                                 "France" = "#c2b7af", "Germany" = "#c2b7af", "Hong Kong" = "blue",
                                 "Iran" = "springgreen3", "Japan" = "royalblue3", "Singapore" = "blue",
                                 "Korea, South" = "slateblue3", "Belgium" = "#c2b7af", "Netherlands" = "#c2b7af",
                                 "Norway" = "#c2b7af", "Spain" = "#c2b7af", "Sweden" = "#c2b7af",
                                 "Switzerland" = "#c2b7af", "33% daily rise" = "gray35", "Austria" = "#c2b7af",
                                 "China" = 'red', "Cruise Ship" = 'purple')) +
  #geom_shadowtext(aes(label = paste0(" ",country)), hjust=0, vjust = 0, data = . %>%
  #group_by(country) %>%
  #top_n(1, days_since_100), bg.color = "white") +
  labs(x = "Number of days since 100th case", y = "",
       title = "Total number of COVID-19 Cases per Country"       ) +
  geom_segment(aes(xend = 48, yend = cases), linetype = 2, colour = 'grey') +
  geom_point(size = 2) +
  geom_text(aes(x = 48.1, label = country), hjust = 0) ->
  static_plot

plt <- static_plot +
  transition_reveal(days_since_100) +
  ease_aes('cubic-in-out') +
  labs(subtitle = "Starting at Day on which 100th Case Occurred")

#rendering the animation for gif
final_animation <- animate(plt, nframes = 100, fps = 10,
                           #duration = 30,
                           width = 600,
                           height = 400, renderer = gifski_renderer())

#saving the animation
anim_save('covid_animate.gif', animation = final_animation)
