# slider demo
library(tidyverse)
library(covid19us)
library(slider)
library(lubridate)
library(glue)

## note from https://davisvaughan.github.io/slider/articles/rowwise.html
# state of California
get_states_daily() %>%
  filter(state == "CA") %>%
  select(date, state, positive, death) %>%
  mutate(new_cases = positive -lead(positive)) %>%
  mutate(roll_new_cases7 = slide_dbl(new_cases, mean,
                    .after =6, .complete = TRUE)) %>%
  mutate(new_deaths = death -lead(death)) %>%
  mutate(roll_new_deaths5 = slide_dbl(new_deaths, mean,
                  .after =4, .complete = TRUE)) %>%
  select(date, state, positive, new_cases, roll_new_cases7) %>%  ggplot(aes(date, roll_new_cases7)) +
  geom_point() +
  geom_smooth(span = 0.4) +
  theme_linedraw()+
  labs(title = "Is COVID Escaping Containment in California?",
       subtitle = "7 day rolling average number of daily cases with a loess smoother",
       x = "Date", y = "")

# LA County, with data file downloaded from
# http://dashboard.publichealth.lacounty.gov/covid19_surveillance_dashboard/
# click on counts by date table and download
# # note on May 11, data only complete through may 5
# cases may 11, - 5/5 800, 5/6 537, 5/7 498, 5/8 239
lac <- read_csv("~/Downloads/date_table-3.csv") %>%
  select(date_dt: total_deaths) %>%
  filter(date_dt < ymd("2020-05-09"))

# alternative, from nytimes
covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

covid %>%
  mutate(county=tolower(county),
         state=tolower(state)) %>%
  filter(state == "california") %>%
  filter(!is.na(fips)) %>%
  filter(county == "los angeles") %>%
  select(date, cases, deaths) %>%
  rename(date_dt = date,
         total_deaths = deaths,
         total_cases = cases) ->
lac2

lac2 %>% slice(-(1:(dim(lac2)[1]-7))) %>%
  mutate(roll_new_cases7 = total_cases -lag(total_cases)) ->
lac3

lac2 %>%
  mutate(new_cases = total_cases -lag(total_cases)) %>%
  mutate(roll_new_cases7 = slide_dbl(new_cases, mean,
                        .after =6, .complete = TRUE)) %>%
  mutate(new_deaths = total_deaths -lag(total_deaths)) %>%
  mutate(roll_new_deaths7 = slide_dbl(new_deaths, mean,
           .after =6, .complete = TRUE)) %>%
  select(date_dt, roll_new_cases7, roll_new_deaths7) %>%
  filter(roll_new_cases7 >0) %>%
  ggplot(aes(date_dt, roll_new_cases7)) +
  geom_point() +
  geom_smooth(span = 0.4) +
  geom_point(data = lac3,
             aes(date_dt, roll_new_cases7),
             color = 'maroon') +
  theme_linedraw()+
  labs(title = "Is Los Angeles County Losing COVID-19 Containment?",
       subtitle = glue("7 day rolling average of daily number of cases with a loess smoother\nComplete Data through {Sys.Date()-7}\nRed Points Represent Actual Daily Data through {Sys.Date()-1}, without smoothing"),
       x = "Date", y = "Cases of COVID-19",
       caption = glue("Data from NY Times csv on Github, downloaded on {Sys.Date()}"))

