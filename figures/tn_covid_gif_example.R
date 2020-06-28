library(tidyverse)
library(httr)
library(gghighlight)
library(ggsci)
library(ggmap)
library(choroplethr)
library(choroplethrMaps)
library(gganimate)
library(deldir)
library(magick)
library(av)


#https://github.com/nytimes/covid-19-data

covid=read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

covid %>% 
  mutate(county=tolower(county),
         state=tolower(state))# %>% 
filter(state==state.select)


state.select="louisiana"

gif_name="la_animation.gif"
{
  tennessee <- covid %>% 
    mutate(county=tolower(county),
           state=tolower(state)) %>% 
    filter(state==state.select)
  #filter(str_detect(fips, "^39")) %>% 
  #group_by(county) %>% 
  #filter(date==max(date)) %>% 
  #ungroup() %>% 
  
  
  states <- map_data("state")
  #tn_df=states
  tn_df <- subset(states, region == state.select)
  counties <- map_data("county")
  tn_county <- subset(counties, region == state.select)
  tn_base <- ggplot(data = tn_df, mapping = aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) + 
    geom_polygon(color = "black", fill = "gray")
  
  
  tncare_map=left_join(tennessee %>% rename(subregion=county), tn_county) %>% 
    na.omit()
  
  p=tn_base + theme_nothing() + 
    geom_polygon(data = tncare_map, aes(fill = cases), color = "white") +
    geom_polygon(color = "black", fill = NA)+  # get the state border back on top
    theme_bw() +
    scale_fill_distiller("Confirmed\nCases", palette = "YlOrRd", direction = 1, na.value="#F03B20")+
    labs(title="Total Confirmed COVID-19 Cases",subtitle="{frame_time}",
         caption="Plot by: @etmckinley; Data: The New York Times")+
    theme(text=element_text(size=16),
          axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank())+
    #facet_wrap(~date)
    
    transition_time(date) 
  
  unlink(list.files("./Figures/gif", pattern = "*.png", full.names = T))
  
  #nframes=length(unique(tncare_map$date))
  
  animate(p, device = "png", nframes=length(unique(tncare_map$date)),
          renderer = file_renderer("./Figures/gif", prefix = "gganim_plot", overwrite = TRUE))
  
  
  
  list.files("./Figures/gif", pattern = "*.png", full.names = T) %>% 
    map(image_read) %>% # reads each path file
    image_join() %>% # joins image
    image_animate(fps=4) %>% # animates
    image_write(paste0("./Figures/gif/", gif_name)) 
  
}