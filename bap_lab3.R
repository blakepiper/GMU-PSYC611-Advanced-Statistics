## PSYC611 Lab 3 ##

library(foreign)
library(ggpubr)
library(moments)

setwd("/Users/blakefinnegan/Documents/George Mason/Fall 2019/Stats/Lab/Assignments/Lab 2")
data <- read.spss("/Users/blakefinnegan/Documents/George Mason/Fall 2019/Stats/Lab/Assignments/Lab 2/lab_2_data.sav", to.data.frame = TRUE, add.undeclared.levels = "no")
data_rm = na.omit(data)

## Simple Normality Function for Use with Shapiro.test
normality = function(shapiro_obj){
  if (shapiro_obj$p.value < 0.05){
    print("Not Normal")
  }
  else{
    print("Normal")
  }
}

## Anxiety Normality
anxiety_dens = ggdensity(data_rm$Anxiety, 
          main = "Density plot of Anxiety",
          xlab = "Amount of Anxiety")
anxiety_norm = shapiro.test(data_rm$Anxiety)
anxiety_test = normality(anxiety_norm)
qqnorm(data_rm$Anxiety)
qqline(data_rm$Anxiety, col = "red", lwd = 2)
skewness(data_rm$Anxiety)
kurtosis(data_rm$Anxiety)

## Anxiety Transformations
anxiety = data_rm$Anxiety
anxiety_sqrt = sqrt(anxiety + 1)
anxiety_log = log10(anxiety + 1)
anxiety_inv = 1/(anxiety + 1)

## Anxiety Inverse Normality
anxiety_inv_dens = ggdensity(anxiety_inv, 
                         main = "Density plot of Anxiety",
                         xlab = "Amount of Anxiety")
anxiety_norm_inv = shapiro.test(anxiety_inv)
anxiety_test_inv = normality(anxiety_norm_inv)
qqnorm(anxiety_inv)
qqline(anxiety_inv, col = "red", lwd = 2)
skewness(anxiety_inv)
kurtosis(anxiety_inv)

## Anxiety Log Normality
anxiety_log_dens = ggdensity(anxiety_log, 
                             main = "Density plot of Anxiety",
                             xlab = "Amount of Anxiety")
anxiety_norm_log = shapiro.test(anxiety_log)
anxiety_test_log = normality(anxiety_norm_log)
qqnorm(anxiety_log)
qqline(anxiety_log, col = "red", lwd = 2)
skewness(anxiety_log)
kurtosis(anxiety_log)

## Depression Normality
depression = data_rm$Depression
depression_dens = ggdensity(depression, 
                         main = "Density plot of Depression",
                         xlab = "Amount of Anxiety")
depression_norm = shapiro.test(depression)
depression_test = normality(depression_norm)
qqnorm(depression)
qqline(depression, col = "red", lwd = 2)
skewness(depression)
kurtosis(depression)

## Depression Log Transformation
depression_log = log10(depression)

## Depression Log Normality
depression_dens = ggdensity(depression_log, 
                            main = "Density plot of Depression",
                            xlab = "Amount of Anxiety")
depression_norm_log = shapiro.test(depression_log)
depression_test = normality(depression_norm_log)
qqnorm(depression_log)
qqline(depression_log, col = "red", lwd = 2)
skewness(depression_log)
kurtosis(depression_log)


