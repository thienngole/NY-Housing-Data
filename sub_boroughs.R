pacman::p_load("tigris", "leaflet", "sp", "maptools", "broom", "httr", "rgdal", "dplyr", "ggplot2","tidyr")
## Data ##########
nyc_neighborhoods <- readOGR("Community Districts.geojson")
summary(nyc_neighborhoods)
### Example plot ###########
leaflet(nyc_neighborhoods) %>%
  addTiles() %>% 
  addPolygons(popup = ~boro_cd) %>%
  addProviderTiles("CartoDB.Positron")
### Read in data and format ###########
data17 <- get_nychsv(year=2017) 
data17$pqi <- all_data[all_data$year==2017, "pqi"]
data17 %>% mutate(borough = recode(borough,
                                   `1` = 2,
                                   `3` = 1,
                                   `2` = 3,
                                   `4` = 4,
                                   `5` = 5)) -> data17
data17 %>% filter(sub<10) %>%
  unite(boro_cd,borough, sub, sep="0") %>% select(boro_cd,pqi) -> sub17_1
data17 %>% filter(sub>=10) %>%
  unite(boro_cd,borough, sub, sep="") %>% select(boro_cd,pqi) -> sub17_2
sub17_bind <- bind_rows(sub17_1,sub17_2)
sub17_bind %>% group_by(boro_cd) %>% summarise(pqi=mean(pqi)) ->sub17
# prep
map_data <- geo_join(nyc_neighborhoods, sub17, "boro_cd", "boro_cd")
## spatial map
pal <- colorNumeric(palette = "Reds",
                    domain = range(map_data@data$pqi, na.rm=T))
## Plot #########
leaflet(map_data) %>%
  addTiles() %>% 
  addPolygons(smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(pqi), popup = ~boro_cd) %>% 
  addProviderTiles("CartoDB.Positron")
