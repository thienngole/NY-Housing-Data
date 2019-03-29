get_nychsv <- function(year=2017, nrows=-1, col_keys = F){
  temp <- tempfile()
  url <- paste0("https://www1.nyc.gov/assets/hpd/downloads/misc/NYCHVS-",year,"-Occupied-File-for-ASA-Challenge-CSV.zip")
  download.file(url, destfile = temp)
  header <- read.csv(unzip(temp), nrows = 1)
  if (!col_keys){
    temp_data <- read.csv(unzip(temp), skip=1, stringsAsFactors = F, nrows=nrows)
    colnames(temp_data) <- colnames(header)
  }
  temp_name <- paste("NYCHVS",year,"Occupied File for ASA Challenge_CSV.csv")
  if (file.exists(temp_name)) 
    file.remove(temp_name)
  if(!col_keys)
    return(temp_data)
  return(header)
}
data17 <- get_nychsv(year = 2017)
data14 <- get_nychsv(year = 2014)
data11 <- get_nychsv(year = 2011)
data08 <- get_nychsv(year = 2008)
data05 <- get_nychsv(year = 2005)
data02 <- get_nychsv(year = 2002)
data99 <- get_nychsv(year = 1999)
data96 <- get_nychsv(year = 1996)
data93 <- get_nychsv(year = 1993)
data91 <- get_nychsv(year = 1991)

## col_keys returns the column headers and descriptions.
## Taking the transpose gives a nice key for reference.
library(dplyr)
get_nychsv(year = 1991, col_keys = T) %>% t() %>% head(n=12)
