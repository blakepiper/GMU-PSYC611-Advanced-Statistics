## PSYC611 Lab 2 ##

library(foreign)
library(pastecs)
setwd("/Users/blakefinnegan/Documents/George Mason/Fall 2019/Stats/Lab/Assignments/Lab 2")
data <- read.spss("/Users/blakefinnegan/Documents/George Mason/Fall 2019/Stats/Lab/Assignments/Lab 2/lab_2_data.sav", to.data.frame = TRUE, add.undeclared.levels = "no")
# Missing Data
data_missing_rows = data[!complete.cases(data),]
data_no_missing = na.omit(data)
n_missing_rows = nrow(data) - nrow(data_no_missing)

# Descriptives
descriptives <- data.frame(stat.desc(data_no_missing))

# Normality Check
hist(data_no_missing$Friends)
hist(data_no_missing$Depression)
hist(data_no_missing$Anxiety)
