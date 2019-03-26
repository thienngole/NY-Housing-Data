temp08 <- tempfile()
temp11 <- tempfile()
url08 <- "https://www1.nyc.gov/assets/hpd/downloads/misc/NYCHVS-2008-Occupied-File-for-ASA-Challenge-CSV.zip"
url11 <- "https://www1.nyc.gov/assets/hpd/downloads/misc/NYCHVS-2011-Occupied-File-for-ASA-Challenge-CSV.zip"
download.file(url08, destfile = temp08)
download.file(url11, destfile = temp11)
data08 <- read.csv(unzip(temp08), skip=1, stringsAsFactors = F)
data11 <- read.csv(unzip(temp11), skip=1, stringsAsFactors = F)
header08 <- read.csv(unzip(temp08), nrows = 1)
header11 <- read.csv(unzip(temp11), nrows = 1)
colnames(data08) <- colnames(header08)
colnames(data11) <- colnames(header11)
### Select Condition Columns ########
library(dplyr)
# t(header08) # this gives a key for colnames
walls08 <- paste0("X_d", 1:4)
win08 <- paste0("X_e", 1:3)
stair08 <- paste0("X_f", c(1,2,4,5))
floor08 <- paste0("X_g", 1:4)
conditions <- c(walls08,win08,stair08,floor08)
# t(header11)
walls11 <- walls08 #same
win11 <- win08
stair11 <- stair08
floor11 <- floor08
### pick the rest ##########
c("borough",
  conditions,
  paste0("X",c("_24a","_25c","_26a","_30a","_31b","_32b","_35a","_36a","_37a","_38a")),
  "hhinc","year") -> cols_trim
data08 %>% select(cols_trim) -> trim08
data11 %>% select(cols_trim) -> trim11
colnames(trim08) == colnames(trim11)
merge_08_11 <- bind_rows(trim08,trim11)
head(merge_08_11)
merge_08_11 %>% mutate_at(vars(conditions),recode,
                          `1` = 1,
                          `8` = 0,
                          `9` = 0) %>%
  mutate(X_36a = recode(X_36a, `1`=1, `2`=0, `8`=0)) %>%
  mutate(X_37a = recode(X_37a, `0`=1, `1`=0, `8`=0)) %>%
  mutate(cond_wall = 2*(X_d1 + X_d2 + X_d3 + X_d4 + X_36a + X_37a)) %>%
  mutate(cond_win = 2*(X_e1 + X_e2 + X_e3)) %>%
  mutate(cond_stair = 2*(X_f1 + X_f2 + X_f4 + X_f5)) %>%
  mutate(cond_floor = 2*(X_g1 + X_g2 + X_g3 + X_g4)) %>%
  mutate(cond_score = cond_wall + cond_win + cond_stair + cond_floor) %>%
  select(-one_of(conditions)) -> merge_08_11_temp
merge_08_11_temp %>% group_by(year, borough) %>%
  summarise(max(cond_score), quantile(cond_score, .90),  mean(cond_score))
## Order and rename columns
col_order <- c("year", "borough", "X_30a", "X_31b", "X_26a", "X_25c", "X_32b",
               "X_24a", "cond_wall", "cond_floor", "cond_win", "cond_stair",
               "X_35a", "X_38a")
merge_08_11_full <- merge_08_11_temp[,col_order]         
