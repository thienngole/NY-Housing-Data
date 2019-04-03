#Run code until line 30
library(plyr)
library(rowr)

my.file <- file.choose()
# Read in data only (no headers -- skip the first two rows):
nych1996 <- read.csv(my.file, skip = 2, header = FALSE, stringsAsFactors = FALSE)
# Read in data (with both rows of headers), temporary data frame:
tmp <- read.csv(my.file, header = TRUE, stringsAsFactors = FALSE)
# Use headers from tmp for nych17:
names(nych1996) <- names(tmp)
# Remove the temporary data frame:
rm(tmp)
#----------------------
#1999
my.file <- file.choose()
# Read in data only (no headers -- skip the first two rows):
nych1999 <- read.csv(my.file, skip = 2, header = FALSE, stringsAsFactors = FALSE)
# Read in data (with both rows of headers), temporary data frame:
tmp <- read.csv(my.file, header = TRUE, stringsAsFactors = FALSE)
# Use headers from tmp for nych1999:
names(nych1999) <- names(tmp)
# Remove the temporary data frame:
rm(tmp)
#======================
revolver <- cbind.fill(nych1996, nych1999, by="X_30a")#this has duplicated colums because it has 2 data sets
names(revolver)[80]<-"new_name1996_X_30a"
names(revolver)[242]<-"new_name1999_X_30a"# that is how i changed from object to new name
ggplot(.data = revolver ) + geom_point(mapping = aes(x="new_name1996_X_30a", y = "new_name1999_X_30a"))

ggplot(.data = nych1996 ) + geom_bar(mapping = aes(x="new_name1996_X_30a"))

ggplot(data = revolver) +
       geom_bar(mapping = aes(x = new_name1996_X_30a, fill = new_name1996_X_30a))#if i try this it does not work

dat <- data.frame(
       years = factor(c("nych1996$X_30a","nych1999$X_30a")))
 ggplot(data = dat, aes(x=years)) +geom_bar()
#======================

#info <- merge(nych1996, nych1999, by ="X_30a")#is too heavy to work

#info <-rbind.fill(nych1996, nych1999)#data frame. this works but everything is under the same variable
#df <- Map(cbind.fill, nych1996, nych1999, MoreArgs = list(fill=NA))#this is a list
#View(df[["X_e1"]])# to view 2 objects
#p <- as.data.frame(df) # data.frame
#ggplot(.data = p) + geom_bar(aes(x= df[["X_30"]], y = p[["X_30a"]]))
# p[["X_30a"]] # is NULL
revolver <- cbind.fill(nych1996, nych1999, by="X_30a")#this line works as expected. Now we have duplicated columns


revolver2 <- cbind.fill(nombre1 = nych1996$X_30a, nombre2 = nych1999$X_30a)
#> ggplot(.data = revolver2) + geom_point(aes(x= nombre1, y = nombre2))
#Error in FUN(X[[i]], ...) : object 'nombre1' not found

#ggplot(.data = revolver2) + geom_point(aes(x= nych1996$X_30a, y = nych1999$X_30a))
#Error: Aesthetics must be either length 1 or the same as the data (15752): y

#ggplot(.data = revolver2) + geom_point(aes(x= nych1996$X_30a, y = "X_30a"))
#the graph shows something but is not even usefull

#ggplot(revolver2, aes(x=nych1996$X_30a, y = "X_30a")) + geom_bar(stat = "identity")
#Error: `data` must be uniquely named but has duplicate columns
#Call `rlang::last_error()` to see a backtrace

#ggplot(revolver2, aes(x=nych1996$X_30a, y = nych1999X_30a)) + geom_bar(stat = "identity")
#Error: `data` must be uniquely named but has duplicate columns
#Call `rlang::last_error()` to see a backtrace

dframe <- rename.vars(dfpartedos, 
from = c(nych1996$X_30a, nych1999$X_30a), to =c("ny96", "ny99"))

names(revolver)[2]<-"new_name1999_X_30a"# that is how i changed from object to new name
ggplot(.data = revolver ) + geom_point(mapping = aes(x="new_name1996_X_30a", y = "new_name1999_X_30a"))
#nuevoo<-rename(revolver, c("object"="ny96", "object"="ny99"))
#The following `from` values were not present in `x`: object
#Warning message:
#The plyr::rename operation has created duplicates for the following name(s): (`ny96`)


#> ggplot(.data = revolver ) + geom_point(aes(x=new_name1996_X_30a, y = new_name1999_X_30a)


#Error in FUN(X[[i]], ...) : object 'new_name1996_X_30a' not found
#> names(revolver)
#[1] "new_name1996_X_30a" "new_name1999_X_30a"