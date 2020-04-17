## Lab 6
setwd("/Users/blakefinnegan/Documents/George Mason/Fall 2019/Stats/Lab/Assignments/Lab 6")
data <- read.csv("lab6_data.csv")

model <- lm(data$Problems ~ data$Money)
summary(model)

plot(data$Money, data$Problems)
abline(lm(data$Problems ~ data$Money), col = 'red')