## Lab 8 ANOVA
library(foreign)
library(HH)
library(ggpubr)
library(PerformanceAnalytics)
library(dplyr)
library(datarium)
library(broom)
library(rstatix)
setwd("/Users/blakefinnegan/Documents/George Mason/Fall 2019/Stats/Lab/Assignments/Lab 8")
anova_data = read.spss("lab8_anova.sav", to.data.frame = TRUE)
ancova_data = read.spss("lab8_ancova.sav", to.data.frame = TRUE)

## ANOVA Assumptions
# Normality
no_incent = anova_data$Grade[anova_data$Incentive=="No incentive"]
hist(no_incent)
shapiro.test(no_incent)
punish = anova_data$Grade[anova_data$Incentive=="Punishment"]
hist(punish)
shapiro.test(punish)
reward = anova_data$Grade[anova_data$Incentive=="Reward"]
hist(reward)
shapiro.test(reward)

# Homogeneity of Variance
hov(Grade ~ Incentive, data = anova_data)
hovPlot(Grade ~ Incentive, data = anova_data)

## One-way ANOVA
group_visualization = ggboxplot(anova_data, x = "Incentive", y = "Grade", 
          color = "Incentive", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("No incentive", "Reward", "Punishment"),
          ylab = "Grade", xlab = "Incentive")

aov = aov(Grade ~ Incentive, data=anova_data)
summary(aov)
TukeyHSD(aov)
