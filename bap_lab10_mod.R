# Lab 10 Moderation
library(foreign)
library(dplyr)
setwd("/Users/blakefinnegan/Documents/George Mason/Fall 2019/Stats/Lab/Assignments/Lab 10")
data <- read.spss("lab10_data_mod.sav", to.data.frame = TRUE)
frame <- data
frame$coa <- recode_factor(frame$coa, "No" = 0, "Yes" = 1)
frame$coa <- as.numeric(frame$coa)
frame$int <- frame$peer_c*frame$coa

# Hierarchical Regression Step 1
model_1 <- lm(alcuse ~ peer_c, frame)
summary(model_1)

# Hierarchical Regression Step 2
model_2 <- lm(alcuse ~ peer_c + int, frame)
summary(model_2)

# Moderation?
mod_test <- anova(model_1, model_2)
mod_test

# Delta R
summary(model_2)$adj.r.squared - summary(model_1)$adj.r.squared


## We have evidence of moderation. 5% of the variance in alcohol use is explained by the interaction between
## peer alcohol use and being a child of an alcoholic. 