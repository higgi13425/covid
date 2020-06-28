# generic function for state gif by county

library(tidyverse)
library(ggmap)
library(gganimate)

animate_state_covid <- function(state_name) {

# select the state, make lower case
state_select <-  state_name %>%
  tolower()

# pull in covid date from nytimes
covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# filter covid to state_select counties
covid %>%
  mutate(county=tolower(county),
         state=tolower(state)) %>%
  filter(state == state_select) ->
  covid_state

# set up end date, start date, number of days of data
end_date <- max(covid_state$date)
start_date <- min(covid_state$date)
days = as.numeric(1+ end_date - start_date)

# make a dataframe of map data by county, adjust varnames
state_df <- map_data("county", region = state_select) %>%
  rename(state = region) %>%
  rename(county = subregion)

# set up a base (gray counties) map
state_base <- ggplot(state_df,
  mapping = aes(x=long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")

# join covid data to map data
state_covid_df <- left_join(covid_state, state_df) %>%
  na.omit()

# begin with a static plot
plot <- state_base +
  theme_nothing() +
  geom_polygon(data = state_covid_df,
               aes(fill = cases), color = "white") +
  geom_polygon(color = "black", fill = NA) +  # get the state border back on top
  scale_fill_gradientn(colours = rev(heat.colors(7)),
                       trans = "log10",
                       name = "COVID-19\nCases") +
  labs(caption="Data: The New York Times") +
  theme(text=element_text(size=16),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  theme(legend.position = "top")

# animation settings
anim <- plot +
  transition_time(date) +
  ease_aes('cubic-in-out') +
  ggtitle("{days} Days of COVID-19 Case Spread in {str_to_title(state_select)}", subtitle="{frame_time}")

# render animation with an end pause
animate(anim, renderer = gifski_renderer(),
        end_pause = 15)

}


# save animation as a named gif in figures folder
# can move this into function if desired.
anim_save(paste0("figures/", state_select, "_animation_cases","_", end_date, ".gif"))

