source("get_nychsv.R")
header <- list()
for (i in c(1991,seq(1993,2017,3))){
  header[[as.character(i%%100)]] <- get_nychsv(year=i,col_keys = T)
}
### Creating column vectors ##############
cond_col <- paste0("X_",c(paste0("d",1:4), paste0("e",1:3), paste0("g", 1:4),paste0("f",c(1,2,4,5))))
num_col <- paste0("X_",c("30a", "31b", "32b", "26a", "25c", "24a", "35a","36a", "37a" ,"38a"))
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
data91 %>% mutate(X_f4 = 9, X_f5=9, X_31b= 999999) %>%
  select(all_col) -> trim_91
data93 %>% mutate(X_f4 = 9, X_f5=9, X_31b= 999999) %>%
  select(all_col) -> trim_93
data96 %>% select(all_col) -> trim_96
data99 %>% select(all_col) -> trim_99
data02 %>% select(all_col) -> trim_02
data05 %>% select(all_col) -> trim_05
data08 %>% select(all_col) -> trim_08
data11 %>% select(all_col) -> trim_11
data14 %>% select(all_col) -> trim_14
data17 %>% rename(X_d1 = X_d12, X_g1 = X_g12) %>%
  mutate(X_d2 = 9, X_g2=9) %>% select(all_col) -> trim_17

all_data <- bind_rows(trim_91, trim_93, trim_96, trim_99, 
                      trim_02, trim_05, trim_08, trim_11, 
                      trim_14, trim_17)

### Index Calculation ###
all_data %>% mutate_at(vars(cond_col),recode,
                          `1` = 1,
                          `8` = 0,
                          `9` = 0) %>%
  mutate_at(vars(X_35a, X_36a, X_38a), recode,`1`=1, `2`=0, `8`=0) %>%
  mutate(X_37a = recode(X_37a, `0`=1, `1`=0, `8`=0)) %>%
  mutate(cond_wall = 2*(X_d1 + X_d2 + X_d3 + X_d4 + X_36a + X_37a)) %>%
  mutate(cond_windows = 2*(X_e1 + X_e2 + X_e3)) %>%
  mutate(cond_stairway = 2*(X_f1 + X_f2 + X_f4 + X_f5)) %>%
  mutate(cond_floor = 2*(X_g1 + X_g2 + X_g3 + X_g4)) %>%
  mutate(cond_score = cond_wall + cond_windows + cond_stairway + cond_floor + 3*(X_35a+X_38a)) %>%
  mutate(kitchen_score = 2*X_26a) %>%
  mutate(toilet_score = recode(X_25c, `1` = 3, `2` = 0, `3` = 5,`8` = 0, `9` = 5)) %>%
  mutate(heat_score = 2*recode(X_32b,`2`=1, `3`=2, `4`=3, `5`= 4, `8` = 0, `9` = 0)) %>%
  mutate(facilites_score = heat_score + toilet_score + kitchen_score) %>% 
  mutate(total_score = cond_score + facilites_score) %>%
  ggplot() + geom_boxplot(aes(y=total_score)) + facet_wrap(facets = ~borough)
  
