pacman::p_load("tigris", "leaflet", "sp", "maptools", "broom", "httr", "rgdal", "dplyr", "ggplot2","tidyr","mapview")
## Data ##########
nyc_neighborhoods <- readOGR("Community Districts.geojson")

### Read in data and format ###########
data17 <- get_nychsv(year=2017) 
data17[which(data17$hhinc==9999999),"hhinc"] <- NA
data17$pqi <- all_data[all_data$year==2017, "pqi"]
data17 %>% mutate(borough = recode(borough,
                                   `1` = 2,
                                   `3` = 1,
                                   `2` = 3,
                                   `4` = 4,
                                   `5` = 5)) -> data17

###### Fuckery ###############
rcd <- function(key,replace) {
  temp_boro[temp_boro == key] <<- replace
}
nyc_neighborhoods$boro_cd -> temp_boro
temp_boro[temp_boro==102] <- 101
temp_boro[temp_boro==103] <- 102
temp_boro[temp_boro==104] <- 103
temp_boro[temp_boro==105] <- 103
temp_boro[temp_boro==106] <- 104
temp_boro[temp_boro==107] <- 105
temp_boro[temp_boro==108] <- 106
temp_boro[temp_boro==109] <- 107
temp_boro[temp_boro==110] <- 108
temp_boro[temp_boro==111] <- 109
temp_boro[temp_boro==112] <- 110
### More fuckery ###########
rcd(202,201)
rcd(203,202)
rcd(204,203)
rcd(205,204)
rcd(206,202)
rcd(207,205)
rcd(208,206)
rcd(202,201)
rcd(209,207)
rcd(210,208)
rcd(211,209)
rcd(212,210)


nyc_neighborhoods$boro_cd <- temp_boro
## Less fuckery
# prep
data17 %>% filter(sub<10) %>%
  unite(boro_cd,borough, sub, sep="0") %>% select(boro_cd,pqi) -> sub17_1
data17 %>% filter(sub>=10) %>%
  unite(boro_cd,borough, sub, sep="") %>% select(boro_cd,pqi) -> sub17_2
sub17_bind <- bind_rows(sub17_1,sub17_2)
sub17_bind %>% group_by(boro_cd) %>% summarise(pqi=mean(pqi)) -> sub17
map_data <- geo_join(nyc_neighborhoods, sub17, "boro_cd", "boro_cd")
## spatial map
pal <- colorNumeric(palette = "Reds",
                    domain = range(map_data@data$pqi, na.rm=T))
## Plot #########
B<-leaflet(map_data) %>%
  addTiles() %>% 
  addPolygons(smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(pqi), popup = ~boro_cd) %>% 
  addProviderTiles("CartoDB.Positron")





data17 %>% filter(sub<10) %>%
  unite(boro_cd,borough, sub, sep="0") %>% select(boro_cd,hhinc) -> sub17_1
data17 %>% filter(sub>=10) %>%
  unite(boro_cd,borough, sub, sep="") %>% select(boro_cd,hhinc) -> sub17_2
sub17_bind <- bind_rows(sub17_1,sub17_2)
sub17_bind %>% group_by(boro_cd) %>% summarise(pqi=mean(hhinc, na.rm=T)) -> sub17



map_data <- geo_join(nyc_neighborhoods, sub17, "boro_cd", "boro_cd")
## spatial map
pal <- colorNumeric(palette = "Blues",
                    domain = range(map_data@data$pqi, na.rm=T),
                    reverse = T)
## Plot #########
A<-leaflet(map_data) %>%
  addTiles() %>% 
  addPolygons(smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(pqi), popup = ~boro_cd) %>% 
  addProviderTiles("CartoDB.Positron")
sync(A,B)