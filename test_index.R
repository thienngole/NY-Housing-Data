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
all_data[which(all_data$hhinc==9999999),"hhinc"] <- NA
### Frequency dist ####
all_data %>% group_by(year, pqi) %>% summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x=pqi,y=percent)) + geom_line(aes(col=factor(year))) + 
  scale_y_continuous(labels=percent) -> g 
ggplotly(g)
### Table #########
all_data %>% group_by(year) %>% summarise(`Index > 0` = sum(pqi > 0)/n(), 
                                          `Index of 1-10` = sum(pqi %in% 1:10)/n(),
                                          `Index of 11-20`= sum(pqi %in% 11:20)/n(),
                                          `Index > 20` = sum(pqi>20)/n()) %>% 
  gather(Group, Percent, -one_of("year")) %>% rename(Year = year) %>% 
  mutate(Group=factor(Group, levels = c("Index > 0", "Index of 1-10", "Index of 11-20", "Index > 20"))) %>%
  ggplot(aes(x=Year, y=Percent)) + geom_col(aes(fill=Group), position="dodge") + 
  facet_wrap(facets = ~Group, ncol = 4) + scale_x_reverse(breaks = seq(1990,2017,3)) + 
  scale_y_continuous(labels = percent) + coord_flip() + theme_bw() + 
  theme(legend.position="none") -> t
ggplotly(t)
### Trend Lines ############
all_data %>% group_by(year) %>% summarise(p_99 = quantile(pqi,.99),
                                          p_95 = quantile(pqi,.95),
                                          p_90 = quantile(pqi,.90),
                                          p_75 = quantile(pqi,.75),
                                          Mean = mean(pqi),
                                          Median = median(pqi)) %>% ungroup(year) %>% 
  gather(line, value, c(p_99,p_95,p_90,p_75,Mean,Median)) %>% 
  mutate(line=factor(line,levels=c("p_99","p_95","p_90","p_75","Mean","Median"))) %>%
  ggplot(aes(x=year, y=value, col =line)) + geom_line(size=1) +
  scale_color_d3(name="", labels=c(paste0(c(99,95,90,75),"th Percentile"),"Mean", "Median")) + 
  theme_bw() + scale_x_continuous(breaks = seq(1990,2017,3)) + ylab("Index Score") + 
  ggtitle("Index Trends 1991-2017") + xlab("") -> g1
ggplotly(g1)
## Testing against hhinc #########
all_data %>% filter(oop_rent < 75000) %>%
ggplot(aes(x=pqi,y=oop_rent)) + 
  geom_point(aes(col=factor(n_room)), position = "jitter", alpha = .4) + 
  facet_wrap(facets = ~borough) -> t1
## Income eda #########
all_data %>% select(pqi, hhinc) %>% filter(!is.na(hhinc)) %>% filter(pqi>0) %>%
    mutate(hhinc_part = case_when(hhinc %in% min(hhinc):quantile(hhinc,.25) ~ "q1",
                                  hhinc %in% (quantile(hhinc,.25)+1):quantile(hhinc,.50) ~ "q2",
                                  hhinc %in% (quantile(hhinc,.50)+1):quantile(hhinc,.75) ~ "q3",
                                  hhinc %in% (quantile(hhinc,.75)+1):max(hhinc) ~ "q4")) %>% 
    ggplot() + geom_boxplot(aes(x=hhinc_part, y=pqi)) -> g4
  g4 %>% ggplotly()
  all_data %>% select(pqi, hhinc) %>% filter(!is.na(hhinc)) %>%
    mutate(pqi_part = case_when(pqi %in% (quantile(pqi,.50)+1):quantile(pqi,.75) ~ "q1",
                                  pqi %in% (quantile(pqi,.75)+1):quantile(pqi,.90) ~ "q2",
                                  pqi %in% (quantile(pqi,.90)+1):quantile(pqi,.95) ~ "q3",
                                  pqi %in% (quantile(pqi,.95)+1):max(pqi) ~ "q4")) %>% 
    ggplot() + geom_boxplot(aes(x=pqi_part, y=log(hhinc))) -> g5
  g5 %>% ggplotly()
## Plots ###############   
pqi_table
ggplotly(t)
ggplotly(g)
ggplotly(g1)
t1

