### merger.R from Ahern ###
library(dplyr)
header <- list()
for (i in c(1991,seq(1993,2017,3))){
  header[[as.character(i%%100)]] <- get_nychsv(year=i,col_keys = T)
}
### Creating column vectors ##############
cond_col <- paste0("X_",c(paste0("d",1:4), paste0("e",1:3), paste0("g", 1:4),paste0("f",c(1,2,4,5))))
num_col <- paste0("X_",c("30a", "31b", "32b", "26c", "25c", "24a", "35a","36a", "37a" ,"38a"))
all_col<- c("year","borough",cond_col, num_col, "hhinc")
### Checking for name differences ########
header$`91` %>% select(one_of(all_col)) %>% t() # No f4, f5, and no 31b
header$`93` %>% select(one_of(all_col)) %>% t() # No f4, f5, and no 31b
header$`96` %>% select(one_of(all_col)) %>% t() # Clean
header$`99` %>% select(one_of(all_col)) %>% t() # Clean
header$`2` %>% select(one_of(all_col)) %>% t()  # Clean
header$`5` %>% select(one_of(all_col)) %>% t()  # Clean
header$`8` %>% select(one_of(all_col)) %>% t()  # Clean
header$`11` %>% select(one_of(all_col)) %>% t() # Clean
header$`14` %>% select(one_of(all_col)) %>% t() # Clean
header$`17` %>% select(one_of(all_col)) %>% t() # (d1,d2),(g1,g2) replaced by d12,g12

### Merging Data ###
data91 %>% mutate(X_f4 = 9, X_f5 = 9, X_31b= 99999) %>%
  select(all_col) -> trim_91
data93 %>% mutate(X_f4 = 9, X_f5=9, X_31b= 99999) %>%
  select(all_col) -> trim_93
data96 %>% select(all_col) -> trim_96
data99 %>% select(all_col) -> trim_99
data02 %>% select(all_col) -> trim_02
data05 %>% select(all_col) -> trim_05
data08 %>% select(all_col) -> trim_08
data11 %>% select(all_col) -> trim_11
data14 %>% select(all_col) -> trim_14
data17 %>% rename(replace=c( "X_d12"= "X_d1",  "X_g12"= "X_g1")) %>%
  mutate(X_d2 = 9, X_g2=9) %>% select(all_col) -> trim_17

## removing uneeded data ##
rm(list= paste0("data", c(91, 93, 96, 99, "02", "05", "08", 11, 14, 17)))
## Merge all the data ##
all_data <- bind_rows(trim_91, trim_93, trim_96, trim_99, 
                      trim_02, trim_05, trim_08, trim_11, 
                      trim_14, trim_17)
rm(list = paste0("trim_", c(91, 93, 96, 99, "02", "05", "08", 11, 14, 17)))
### Recode to Index Calculation ###
all_data %>% mutate_at(vars(cond_col),recode,
                       `1` = 1,
                       `8` = 0,
                       `9` = 0) %>%
  mutate_at(vars(X_35a, X_36a, X_38a), recode,`1`=1, `2`=0, `8`=0) %>%
  mutate(X_37a = recode(X_37a, `0`=1, `1`=0, `8`=0)) %>%
  mutate(cond_wall = 2*(X_d1 + X_d2 + X_d3 + X_d4 + X_36a + X_37a)) %>%
  mutate(cond_windows = (5*X_e1 + 2*X_e2 + 3*X_e3)) %>%
  mutate(cond_stairway = 2*(X_f1 + X_f2 + X_f4 + X_f5)) %>%
  mutate(cond_floor = 2*(X_g1 + X_g2 + X_g3 + X_g4)) %>%
  mutate(cond_score = cond_wall + cond_windows + cond_stairway + cond_floor + 3*(X_35a+X_38a)) %>%
  mutate(kitchen_score = recode(X_26c,`1`=0, `8`=0, `2` = 3, `9` = 5)) %>%
  mutate(toilet_score = recode(X_25c, `1` = 3, `2` = 0, `3` = 5,`8` = 0, `9` = 5)) %>%
  mutate(heat_score = 2*recode(X_32b,`2`=1, `3`=2, `4`=3, `5`= 4, `8` = 0, `9` = 0)) %>%
  mutate(facilites_score = heat_score + toilet_score + kitchen_score) %>% 
  transmute(total_score = cond_score + facilites_score) -> score
all_data$pqi <- score$total_score

###### End of merger.R by ahern###

library(ggplot2)

# replace hhinc = 999999(no income) and 999998(not reported) to 0
all_data$hhinc[all_data$hhinc==999999] <- 0
all_data$hhinc[all_data$hhinc==9999999] <- 0
all_data$hhinc[all_data$hhinc==999998] <- 0

# get household income by total score for 91
hhinc91 <- all_data %>%
  filter(year == "91") %>%
  select(hhinc, pqi) %>%
  group_by(pqi) %>%
  summarise(median_hhinc_91 = median(hhinc, rm.na = TRUE))

# get household income by total score for 93
hhinc93 <- all_data %>%
  filter(year == "93") %>%
  select(hhinc, pqi) %>%
  group_by(pqi) %>%
  summarise(median_hhinc_93 = median(hhinc, rm.na = TRUE))

# get household income by total score for 96
hhinc96 <- all_data %>%
  filter(year == "96") %>%
  select(hhinc, pqi) %>%
  group_by(pqi) %>%
  summarise(median_hhinc_96 = median(hhinc, rm.na = TRUE))

# get household income by total score for 99
hhinc99 <- all_data %>%
  filter(year == "99") %>%
  select(hhinc, pqi) %>%
  group_by(pqi) %>%
  summarise(median_hhinc_99 = median(hhinc, rm.na = TRUE))

# get household income by total score for 2002
hhinc02 <- all_data %>%
  filter(year == "2002") %>%
  select(hhinc, pqi) %>%
  group_by(pqi) %>%
  summarise(median_hhinc_02 = median(hhinc, rm.na = TRUE))

# get household income by total score for 2005
hhinc05 <- all_data %>%
  filter(year == "2005") %>%
  select(hhinc, pqi) %>%
  group_by(pqi) %>%
  summarise(median_hhinc_05 = median(hhinc, rm.na = TRUE))

# get household income by total score for 2008
hhinc08 <- all_data %>%
  filter(year == "2008") %>%
  select(hhinc, pqi) %>%
  group_by(pqi) %>%
  summarise(median_hhinc_08 = median(hhinc, rm.na = TRUE))

# get household income by total score for 11
hhinc11 <- all_data %>%
  filter(year == "2011") %>%
  select(hhinc, pqi) %>%
  group_by(pqi) %>%
  summarise(median_hhinc_11 = median(hhinc, rm.na = TRUE))

# get household income by total score for 93
hhinc14 <- all_data %>%
  filter(year == "2014") %>%
  select(hhinc, pqi) %>%
  group_by(pqi) %>%
  summarise(median_hhinc_14 = median(hhinc, rm.na = TRUE))

# get household income by total score for 17
hhinc17 <- all_data %>%
  filter(year == "2017") %>%
  select(hhinc, pqi) %>%
  group_by(pqi) %>%
  summarise(median_hhinc_17 = median(hhinc, rm.na = TRUE))

# merge
hhinc9117 <- full_join(x = hhinc91, y = hhinc93, by = "pqi") #%>%
hhinc9117 <- full_join(x = hhinc9117, y = hhinc96, by = "pqi") #%>%
hhinc9117 <- full_join(x = hhinc9117, y = hhinc99, by = "pqi") #%>
hhinc9117 <- full_join(x = hhinc9117, y = hhinc02, by = "pqi") ##%>%
hhinc9117 <- full_join(x = hhinc9117, y = hhinc05, by = "pqi") #%>%
hhinc9117 <- full_join(x = hhinc9117, y = hhinc08, by = "pqi") #%>%
hhinc9117 <- full_join(x = hhinc9117, y = hhinc11, by = "pqi") #%>%
hhinc9117 <- full_join(x = hhinc9117, y = hhinc14, by = "pqi") #%>%
hhinc9117 <- full_join(x = hhinc9117, y = hhinc17, by = "pqi") 
  
# convert NA to 0

hhinc9117$median_hhinc_93[is.na(hhinc9117$median_hhinc_93)] <- 0
hhinc9117$median_hhinc_96[is.na(hhinc9117$median_hhinc_96)] <- 0
hhinc9117$median_hhinc_99[is.na(hhinc9117$median_hhinc_99)] <- 0
hhinc9117$median_hhinc_02[is.na(hhinc9117$median_hhinc_02)] <- 0
hhinc9117$median_hhinc_05[is.na(hhinc9117$median_hhinc_05)] <- 0
hhinc9117$median_hhinc_08[is.na(hhinc9117$median_hhinc_08)] <- 0
hhinc9117$median_hhinc_11[is.na(hhinc9117$median_hhinc_11)] <- 0
hhinc9117$median_hhinc_14[is.na(hhinc9117$median_hhinc_14)] <- 0
hhinc9117$median_hhinc_17[is.na(hhinc9117$median_hhinc_17)] <- 0

library(reshape2)
hhinc9117 <- melt(hhinc9117, id = "pqi")

ggplot(data = hhinc9117, mapping = aes(x = pqi, y = value, color = variable))+
  geom_line() +
  scale_x_continuous()+
  scale_y_continuous(name="median household income", labels = scales::comma)+
  scale_color_manual(labels = c("1991", "1993","1996","1999","2002","2005","2008","2011","2014","2017"), values = c("red", "darkred", "orange", "yellow", "lightgreen", "darkgreen", "blue", "purple", "pink", "black") ) +
  labs(title = "Median Household Income by Score Distribution", x = "Total score", color = "Median household income")
