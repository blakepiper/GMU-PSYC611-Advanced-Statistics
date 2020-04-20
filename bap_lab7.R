## Lab 7
library(foreign)
library(PerformanceAnalytics)
library(lm.beta)
library(olsrr)
setwd("/Users/blakefinnegan/Documents/George Mason/Fall 2019/Stats/Lab/Assignments/Lab 7")
data <- read.spss("lab7_data.sav", to.data.frame=TRUE)

## Looking at the Three Variables
plot(data$Stress, data$Problems)
abline(lm(data$Problems ~ data$Stress), col = 'red')

## Multiple Regression
model = lm(Problems ~ Money + Stress, data=data)
model_s = lm.beta(model)
summary(model_s)

## Regression Assumption Check
random = rchisq(nrow(data), 7)
fake = lm(random~., data=data)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)

## Linearity
qqnorm(standardized)
abline(0,1, col = "red")

## Normality
hist(standardized)

## Homogeneity
plot(fitted, standardized)
abline(0,0)
abline(v = 0)

## Collinearity
chart.Correlation(data)
ols_vif_tol(model)

## Outliers

model_stres = rstandard(model)

hist(model_stres)
plot(model_stres)

## Leverage
leverage = hatvalues(model)
lev_cutoff = 3 * (ncol(data) / nrow(data))
table(leverage > lev_cutoff)
hist(leverage)
