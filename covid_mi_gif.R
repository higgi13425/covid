library(tidyverse)
library(ggmap)
library(gganimate)

covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

covid %>% # filter covid down to Michigan counties
  mutate(county=tolower(county),
         state=tolower(state)) %>%
  filter(state == "michigan") ->
covid_mi

end_date <- max(covid_mi$date)

mi_df <- map_data("county", region = "michigan") %>%
  rename(state = region) %>%
  rename(county = subregion)

mi_base <- ggplot(mi_df,
            mapping = aes(x=long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")


mi_covid_df <- left_join(covid_mi, mi_df) %>%
  na.omit()

# static version, faceted
p <- mi_base +
  theme_nothing() +
  geom_polygon(data = mi_covid_df,
               aes(fill = cases), color = "white") +
  geom_polygon(color = "black", fill = NA) +  # get the state border back on top
  theme_bw() +
  scale_fill_distiller("Confirmed\nCases", palette = "YlOrRd", direction = 1, na.value="#F03B20")+
  labs(title="Total Confirmed COVID-19 Cases",
       caption="Plot after: @etmckinley; Data: The New York Times")+
  theme(text=element_text(size=16),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())+
  facet_wrap(~date)

# animated version without facets
plot <- mi_base +
  theme_nothing() +
  geom_polygon(data = mi_covid_df,
               aes(fill = cases), color = "white") +
  geom_polygon(color = "black", fill = NA) +  # get the state border back on top
  scale_fill_gradientn(colours = rev(heat.colors(7)),
                       trans = "log10",
                       name = "COVID-19\nCases") +
  labs(caption="Data: The New York Times") +
  theme(text=element_text(size=16),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  theme(legend.position = c(0.35, 0.3))

anim <- plot +
  transition_time(date) +
  ease_aes('cubic-in-out') +
  ggtitle("21 Days of COVID-19 Case Spread in Michigan",
          subtitle="{frame_time}")

animate(anim, renderer = gifski_renderer(),
        end_pause = 15)

anim_save(paste0("figures/","mi_animation_cases","_", end_date, ".gif"))

