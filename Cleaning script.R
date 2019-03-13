pacman::p_load("dplyr","tidyr","ggplot2", "scales")
### Rename Data ###################
smart_name <- function(x) {
  gsub("(^|[^[:alnum:]])([[:alnum:]])", " \\U\\2", x, perl = TRUE) %>%
    trimws("left")
}
data <- read.csv("house91.csv",skip = 1)
oldNames <- data %>% select(contains("..")) %>% names()
newNames <- gsub("\\.\\.","\\.",oldNames,fixed = F)
smartNames <- data %>% rename_at(vars(oldNames), ~newNames) %>% names()%>% smart_name()
data <- data %>% rename_all(~smartNames)
#
### Recode Data #####################
data %>% mutate(Borough = recode(Borough,
  `1`="Bronx",
  `2`="Brooklyn",
  `3`="Manhattan",
  `4`="Queens", 
  `5`="Staten Island")) -> data
data %>% select(contains("Condition")) %>% 
  select(-one_of("Condition Of Building")) %>% names() -> conditions
data %>% mutate_at(vars(conditions), recode, 
                    `1`="Yes", 
                    `8`= NA_character_, 
                    `9`= "No") -> data
data %>% mutate(`Condition Of Building` = recode(`Condition Of Building`,
         `1` = "Very Poor",
         `2` = "Good",
         `3` = "Deteriorating",
         `8` = NA_character_)) -> data
data %>% mutate_at(vars(contains("Any")), recode,
                   `1` = "Yes",
                   `2` = "No",
                   `8` = NA_character_) -> data
data %>% mutate_at(vars(contains('Sex')), recode,
                   `1` = "Male",
                   `2` = "Female") -> data
data %>% mutate_at(vars(contains("Hispanic")), recode,
                   `1` = "No",
                   `2` = "Puerto Rican",
                   `3` = "Dominican",
                   `4` = "Cuban",
                   `5` = "South American",
                   `6` = "Mexican",
                   `7` = "Other",
                   `8` = NA_character_) -> data
data %>% mutate_at(vars(contains("Race")), recode,
                   `1` = "White",
                   `2` = "Black",
                   `3` = "Native American, Eskimo, or Aleut",
                   `4` = "Chinese",
                   `5` = "Filipino",
                   `6` = "Korean",
                   `7` = "Vietnamese",
                   `8` = "Indian, Pakistani, or Bangladeshi",
                   `9` = "Asian",
                   `10` = "Other",
                   `98` = NA_character_) -> data



