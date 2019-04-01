pacman::p_load("dplyr","tidyr","ggplot2", "scales", "plotly","ggsci")
#
### Rename ## #########
all_data %>% rename(m_rent = X_30a, oop_rent = X_31b, 
                    n_heat = X_32b, fun_kitchen = X_26c,
                    b_toilet = X_25c, n_room = X_25c,
                    rats = X_35a, leak = X_38a) -> all_data
all_data %>% mutate(borough = recode(borough,
                                     `1`="Bronx",
                                     `2`="Brooklyn",
                                     `3`="Manhattan",
                                     `4`="Queens", 
                                     `5`="Staten Island")) %>%
  mutate(year = recode(year, `91`=1991, `93`=1993, `96`=1996, 
                       `99`=1999, `2002`=2002,`2005`=2005,
                       `2008`=2008,`2011`=2011,`2014`=2014,`2017`=2017)) %>%
  mutate(oop_rent = na_if(oop_rent, 99999)) %>%
  mutate(oop_rent = na_if(oop_rent, 99999)) -> all_data
all_data[all_data$hhinc==999999,"hhinc"] <- NA
### Frequency dist ####
all_data %>% group_by(year, pqi) %>% summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x=pqi,y=percent)) + geom_line(aes(col=factor(year))) + 
  scale_y_continuous(labels=percent) -> g 
ggplotly(g)
### Table #########
all_data %>% group_by(year) %>% summarise(perc_0 = sum(pqi > 0)/n(), 
                                          perc_1_10 = sum(pqi %in% 1:10)/n(),
                                          perc_11_20 = sum(pqi %in% 11:20)/n(),
                                          perc_20 = sum(pqi>20)/n()) -> pqi_table
  pqi_table %>% gather(Group, Percent, -one_of("year")) %>%
  ggplot(aes(x=year, y=Percent)) + geom_col(aes(fill=Group), position="dodge") + 
  facet_wrap(facets = ~Group, ncol = 1) + scale_x_continuous(breaks = seq(1990,2017,3)) + 
  scale_y_continuous(labels = percent) -> t
### Trend Lines ############
all_data %>% group_by(year) %>% summarise(p_99 = quantile(pqi,.99),
                                          p_95 = quantile(pqi,.95),
                                          p_90 = quantile(pqi,.90),
                                          p_75 = quantile(pqi,.75),
                                          Mean = mean(pqi),
                                          Median = median(pqi)) %>% ungroup(year) %>% 
  gather(line, value, c(p_99,p_95,p_90,p_75,Mean,Median)) %>% 
  mutate(line=factor(line,levels=c("p_99","p_95","p_90","p_75","Mean","Median"))) %>%
  ggplot(aes(x=year, y=value, col =line)) + geom_line(size=1) + scale_color_d3() + theme_bw() +
  scale_x_continuous(breaks = seq(1990,2017,3)) -> g1
## Testing against hhinc #########
all_data %>% filter(oop_rent < 75000) %>%
ggplot(aes(x=pqi,y=oop_rent)) + 
  geom_point(aes(col=factor(n_room)), position = "jitter", alpha = .4) + 
  facet_wrap(facets = ~borough) -> t1
## Plots ###############   
pqi_table
ggplotly(t)
ggplotly(g)
ggplotly(g1)
t1


data[,col_to_keep]            