## Lab 4 
library(foreign)
setwd("/Users/blakefinnegan/Documents/George Mason/Fall 2019/Stats/Lab/Assignments/Lab 4")
data <- read.spss("Lab HW 4 data.sav", to.data.frame = TRUE)
na_rm <- na.omit(data)


## Control Descriptives
mean(na_rm$Outcome[na_rm$Condition=="Control"])
sd(na_rm$Outcome[na_rm$Condition=="Control"])

## Treatment Descriptives
mean(na_rm$Outcome[na_rm$Condition=="Treatment"])
sd(na_rm$Outcome[na_rm$Condition=="Treatment"])

## Independent Samples T-test
t.test(na_rm$Outcome ~ na_rm$Condition)


## An independent samples t-test revealed a non-significant difference between 
## the control (M = 5, SD = 2.19) and treatment conditions (M = 6, SD = 2.24); 
## t(11) = -.86, p = .4, 95% CI [-3.56, 1.56].


