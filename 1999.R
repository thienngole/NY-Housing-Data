#condition of walls (36a, 37a, d1, d3, d12)
#Anayeli Ochoa

library(dplyr)

# Select the 1999 .csv file, save it:
my.file <- file.choose()
# Read in data only (no headers -- skip the first two rows):
nych1999 <- read.csv(my.file, skip = 2, header = FALSE, stringsAsFactors = FALSE)
# Read in data (with both rows of headers), temporary data frame:
tmp <- read.csv(my.file, header = TRUE, stringsAsFactors = FALSE)
# Use headers from tmp for nych1999:
names(nych1999) <- names(tmp)
# Remove the temporary data frame:
rm(tmp)
#print data nych1999
nych1999

#-------------------------------------------------------------------
#Total Household Income Recode (hhinc)           ?
#data.frame
hhinc = nych1999[c("hhinc")]

#-------------------------------------------------------------------
#Monthly contract rent (30a)                   ?
X_30a = nych1999[c("X_30a")]
#-------------------------------------------------------------------
#Out of pocket rent (31b)                     ?
X_31b = nych1999[c("X_31b")]
#-------------------------------------------------------------------
#Kitchen facilities (26a)                   5 if none
#3=none and that means replace with 5
nych1999$X_26a <- replace(nych1999$X_26a, nych1999$X_26a==0, 0)
nych1999$X_26a <- replace(nych1999$X_26a, nych1999$X_26a==1, 0)
nych1999$X_26a <- replace(nych1999$X_26a, nych1999$X_26a==2, 0)
nych1999$X_26a <- replace(nych1999$X_26a, nych1999$X_26a==3, 5)


X_26a = nych1999[c("X_26a")]

#-------------------------------------------------------------------
#Toilet breakdowns (25c)          3 = 1(Yes)  5 = 9(no plumbing)
nych1999$X_25c <- replace(nych1999$X_25c, nych1999$X_25c==1, 3)
nych1999$X_25c <- replace(nych1999$X_25c, nych1999$X_25c==2, 0)
nych1999$X_25c <- replace(nych1999$X_25c, nych1999$X_25c==3, 0)
nych1999$X_25c <- replace(nych1999$X_25c, nych1999$X_25c==8, 0)
nych1999$X_25c <- replace(nych1999$X_25c, nych1999$X_25c==9, 5)


X_25c = nych1999[c("X_25c")]
#rowSums(X_25c)


#--------------------------------------------------------------------
#Number of heating equipment breakdowns (32b) (2 for each break)
#2 = one time
nych1999$X_32b <- replace(nych1999$X_32b, nych1999$X_32b==2, 2)
nych1999$X_32b <- replace(nych1999$X_32b, nych1999$X_32b==3, 0)
nych1999$X_32b <- replace(nych1999$X_32b, nych1999$X_32b==4, 0)
nych1999$X_32b <- replace(nych1999$X_32b, nych1999$X_32b==5, 0)
nych1999$X_32b <- replace(nych1999$X_32b, nych1999$X_32b==8, 0)
nych1999$X_32b <- replace(nych1999$X_32b, nych1999$X_32b==9, 0)

X_32b <- nych1999[c("X_32b")] #creo q eso es un data fram y ne parentensis es character

#--------------------------------------------------------------------
#Number of rooms (24a)
X_24a = nych1999[c("X_24a")]
#--------------------------------------------------------------------
#condition of walls (36a, 37a, d1, d3, d12)

nych1999$X_36a <- replace(nych1999$X_36a, nych1999$X_36a==0, 2)
nych1999$X_36a <- replace(nych1999$X_36a, nych1999$X_36a==1, 0)
nych1999$X_36a <- replace(nych1999$X_36a, nych1999$X_36a==8, 0)


nych1999$X_37a <- replace(nych1999$X_37a, nych1999$X_37a==0, 0)
nych1999$X_37a <- replace(nych1999$X_37a, nych1999$X_37a==1, 2)
nych1999$X_37a <- replace(nych1999$X_37a, nych1999$X_37a==8, 0)

nych1999$X_d1 <- replace(nych1999$X_d1, nych1999$X_d1 == 1,  2)
nych1999$X_d1 <- replace(nych1999$X_d1, nych1999$X_d1 == 8,  0)
nych1999$X_d1 <- replace(nych1999$X_d1, nych1999$X_d1 == 9,  0)


nych1999$X_d3 <- replace(nych1999$X_d3, nych1999$X_d3 == 1,  2)
nych1999$X_d3 <- replace(nych1999$X_d3, nych1999$X_d3 == 8,  0)
nych1999$X_d3 <- replace(nych1999$X_d3, nych1999$X_d3 == 9,  0)
nych1999 <- mutate(.data = nych1999, X_d12 = 0)

#numeric vector
#condition_wall <- c(nych1999$X_36a, nych1999$X_37a, nych1999$X_d1, nych1999$X_d3, nych1999$X_d12)
#sum rows and put them into a new column \mutate us just for data frames
#matrix
condition_wall <-(cbind(nych1999$X_36a, nych1999$X_37a, nych1999$X_d1, nych1999$X_d3, nych1999$X_d12))
condition_wall <- rowSums(condition_wall)

#-----------------------------------------------------------------------
#Condition of floors (g1, g2, g3, g4, g5)
nych1999$X_g1 <- replace(nych1999$X_g1, nych1999$X_g1==1, 2)
nych1999$X_g1 <- replace(nych1999$X_g1, nych1999$X_g1==8, 0)
nych1999$X_g1 <- replace(nych1999$X_g1, nych1999$X_g1==9, 0)


nych1999$X_g2 <- replace(nych1999$X_g2, nych1999$X_g2==1, 2)
nych1999$X_g2 <- replace(nych1999$X_g2, nych1999$X_g2==8, 0)
nych1999$X_g2 <- replace(nych1999$X_g2, nych1999$X_g2==9, 0)


nych1999$X_g3 <- replace(nych1999$X_g3, nych1999$X_g3==1, 2)
nych1999$X_g3 <- replace(nych1999$X_g3, nych1999$X_g3==8, 0)
nych1999$X_g3 <- replace(nych1999$X_g3, nych1999$X_g3==9, 0)


nych1999$X_g4 <- replace(nych1999$X_g4, nych1999$X_g4==1, 2)
nych1999$X_g4 <- replace(nych1999$X_g4, nych1999$X_g4==8, 0)
nych1999$X_g4 <- replace(nych1999$X_g4, nych1999$X_g4==9, 0)


nych1999$X_g5 <- replace(nych1999$X_g5, nych1999$X_g5==1, 2)
nych1999$X_g5 <- replace(nych1999$X_g5, nych1999$X_g5==8, 0)
nych1999$X_g5 <- replace(nych1999$X_g5, nych1999$X_g5==9, 0)


#condition_floors = nych1999[c("nych1999$X_g1","nych1999$X_g2", "nych1999$X_g3", "nych1999$X_g4", "nych1999$X_g5")]
#rowSums(condition_floors)
#pisos = transmute(.data = condition_floors, floors = rowSums(condition_floors))

condition_floors <-(cbind(nych1999$X_g1, nych1999$X_g2, nych1999$X_g3, nych1999$X_g4, nych1999$X_g5))
condition_floors <- rowSums(condition_floors)


#--------------------------------------------------------------
#Condition of Windows (e1, e2, e3, e4)

nych1999$X_e1 <- replace(nych1999$X_e1, nych1999$X_e1==1, 2)
nych1999$X_e1 <- replace(nych1999$X_e1, nych1999$X_e1==8, 0)
nych1999$X_e1 <- replace(nych1999$X_e1, nych1999$X_e1==9, 0)


nych1999$X_e2 <- replace(nych1999$X_e2, nych1999$X_e2==1, 2)
nych1999$X_e2 <- replace(nych1999$X_e2, nych1999$X_e2==8, 0)
nych1999$X_e2 <- replace(nych1999$X_e2, nych1999$X_e2==9, 0)


nych1999$X_e3 <- replace(nych1999$X_e3, nych1999$X_e3==1, 2)
nych1999$X_e3 <- replace(nych1999$X_e3, nych1999$X_e3==8, 0)
nych1999$X_e3 <- replace(nych1999$X_e3, nych1999$X_e3==9, 0)


nych1999$X_e4 <- replace(nych1999$X_e4, nych1999$X_e4==1, 2)
nych1999$X_e4 <- replace(nych1999$X_e4, nych1999$X_e4==8, 0)
nych1999$X_e4 <- replace(nych1999$X_e4, nych1999$X_e4==9, 0)


#condition_windows = nych1999[c("X_e1","X_e2", "X_e3", "X_e4")]
#rowSums(condition_windows)
#nych1999$condition_windows = transmute(nych1999, windows = rowSums(condition_windows))

condition_windows <-(cbind(nych1999$X_e1, nych1999$X_e2, nych1999$X_e3, nych1999$X_e4))
condition_windows <- rowSums(condition_windows)



#------------------------------------------------------------------
#Condition of Stairways (f1, f2, f3, f4, f5)
nych1999$X_f1 <- replace(nych1999$X_f1, nych1999$X_f1==1, 2)
nych1999$X_f1 <- replace(nych1999$X_f1, nych1999$X_f1==8, 0)
nych1999$X_f1 <- replace(nych1999$X_f1, nych1999$X_f1==9, 0)


nych1999$X_f2 <- replace(nych1999$X_f2, nych1999$X_f2==1, 2)
nych1999$X_f2 <- replace(nych1999$X_f2, nych1999$X_f2==8, 0)
nych1999$X_f2 <- replace(nych1999$X_f2, nych1999$X_f2==9, 0)


nych1999$X_f3 <- replace(nych1999$X_f3, nych1999$X_f3==1, 2)
nych1999$X_f3 <- replace(nych1999$X_f3, nych1999$X_f3==8, 0)
nych1999$X_f3 <- replace(nych1999$X_f3, nych1999$X_f3==9, 0)


nych1999$X_f4 <- replace(nych1999$X_f4, nych1999$X_f4==1, 2)
nych1999$X_f4 <- replace(nych1999$X_f4, nych1999$X_f4==8, 0)
nych1999$X_f4 <- replace(nych1999$X_f4, nych1999$X_f4==9, 0)


nych1999$X_f5 <- replace(nych1999$X_f5, nych1999$X_f5==1, 2)
nych1999$X_f5 <- replace(nych1999$X_f5, nych1999$X_f5==8, 0)
nych1999$X_f5 <- replace(nych1999$X_f5, nych1999$X_f5==9, 0)


#condition_stairways = nych1999[c("X_f1","X_f2","X_f3", "X_f4", "X_f5")]
#rowSums(condition_stairways)
#nych1999$condition_stairways = transmute(nych1999,stairways=rowSums(condition_stairways))

condition_stairways <-(cbind(nych1999$X_f1, nych1999$X_f2, nych1999$X_f3, nych1999$X_f4, nych1999$X_f5))
condition_stairways <- rowSums(condition_stairways)

#------------------------------------------------------------------
#Presence of mice or rats (35a) | 3 for Yes
nych1999$X_35a <- replace(nych1999$X_35a, nych1999$X_35a==1, 3)
nych1999$X_35a <- replace(nych1999$X_35a, nych1999$X_35a==2, 0)
nych1999$X_35a <- replace(nych1999$X_35a, nych1999$X_35a==8, 0)

X_35a = nych1999[c("X_35a")]
#-------------------------------------------------------------------
#Water leakage inside apartment (38a) | 3 for Yes
nych1999$X_38a <- replace(nych1999$X_38a, nych1999$X_38a==1, 3)
nych1999$X_38a <- replace(nych1999$X_38a, nych1999$X_38a==2, 0)
nych1999$X_38a <- replace(nych1999$X_38a, nych1999$X_38a==8, 0)

X_38a = nych1999[c( "X_38a")]


#===================================================================
#printing final data


library(dplyr)
new_nych1999 <- cbind(hhinc, X_30a, X_31b, X_26a, X_25c,X_32b,
                      X_24a, condition_wall, condition_floors, condition_windows,condition_stairways, X_35a, X_38a)

new_nych1999

write.csv(new_nych1999, file = "new_nych1999.csv")



