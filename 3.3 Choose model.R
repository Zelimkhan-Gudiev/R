remove(list = ls())
rm()

#### Packages and librarys. ____________________________________________________________________________________________________ ####

library(ggplot2)
library(psych)
library(dplyr)
library(readxl)

#### Data preprocessing _______________________________________________________________________________________________________ ####

getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF/")
setwd("/Users/zelimkhan/Desktop/Data/GitHub/DF/")#

yt <- read.csv2("yt.csv")
yt <- read_xlsx("plan.xlsx")

swiss <- swiss
names(swiss)
str(swiss)
summary(swiss)
fit_full <- lm(Fertility ~ ., data = swiss)
summary(fit_full)
fit_reduced1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, data = swiss)
summary(fit_reduced1)

anova(fit_full, fit_reduced1)

fit_reduced2 <- lm(Fertility ~ Infant.Mortality + Agriculture + Catholic + Education, data = swiss)
summary(fit_reduced2)

anova(fit_full, fit_reduced2)

# model selection

optimal_fit <- step(fit_full, direction = 'backward')
summary(optimal_fit)


####  Step 4  of 6 ####
attitude <- attitude
names(attitude)
str(attitude)
model_full <- lm(rating ~ ., data = attitude)
summary(model_full)
model_null <- lm(rating ~ 1, data = attitude)
summary(model_null)

scope = list(lower = model_null, upper = model_full)

ideal_model <- step(model_null, scope = list(lower = model_null, upper = model_full), direction = 'forward')
summary(ideal_model)

####  Step 5  of 6 ####
ideal_model <- lm(formula = rating ~ complaints + learning, data = attitude)
anova(model_full, ideal_model)


####  Step 6  of 6 ####

lcs <- LifeCycleSavings
str(lcs)

fit6 <- lm(sr ~ (.)^2, lcs)
summary(fit6)
step(fit6, direction = 'backward')


