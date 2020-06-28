# experiment with rayshader

library(rgl)
library(rgdal)
library(rayshader)
library(rayrender)
library(tidyverse)
library(sf)
library(covid19us)
library(maps)

# diamonds
ggdiamonds = ggplot(diamonds) +
  stat_density_2d(aes(x = x, y = depth, fill = stat(nlevel)),
                  geom = "polygon", n = 100, bins = 10, contour = TRUE) +
  facet_wrap(clarity~.) +
  scale_fill_viridis_c(option = "A")

par(mfrow = c(1, 2))

plot_gg(ggdiamonds, width = 5, height = 5, raytrace = FALSE, preview = TRUE)
plot_gg(ggdiamonds, width = 5, height = 5, multicore = TRUE, scale = 250,
        zoom = 0.7, theta = 10, phi = 30, windowsize = c(800, 800))
render_snapshot(clear = TRUE)


# mtcars
mtplot = ggplot(mtcars) +
  geom_point(aes(x = mpg, y = disp, color = cyl)) +
  scale_color_continuous(limits = c(0, 8))

plot_gg(mtplot, width = 3.5, multicore = TRUE, windowsize = c(800, 800),
        zoom = 0.75, phi = 35, theta = 30, sunangle = 225, soliddepth = -100)
Sys.sleep(0.2)
render_snapshot(clear = TRUE)

# us states
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)

state_abb_table <- tibble(state.name, state.abb) %>%
  purrr:: set_names('state', 'abb') %>%
  mutate(state = tolower(state))

states <- left_join(states, state_abb_table, by = c("ID" = "state"))

states_map <- states %>%
  left_join(covid19us::get_states_current(), by = c("abb" = "state")) %>%
  select(abb, positive, geom) %>%
  filter(!is.na(abb)) %>%
  ggplot() +
  geom_sf(aes(fill = positive)) +
  theme_void() +
  theme(legend.position = "none")

plot_gg(states_map, width = 3.5, multicore = TRUE, windowsize = c(800, 800),
        zoom = 1, phi = 35, theta = 30, sunangle = 225, soliddepth = -100)
Sys.sleep(0.2)
render_snapshot(clear = TRUE)

