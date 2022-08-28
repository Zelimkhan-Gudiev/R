remove(list = ls())
rm()

#### Packages and librarys. ____________________________________________________________________________________________________ ####

library(ggplot2)
library(psych)
library(dplyr)
library(psych)
library(readxl)

#### Data preprocessing _______________________________________________________________________________________________________ ####

getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF/")
setwd("/Users/zelimkhan/Desktop/Data/GitHub/DF/")#

yt <- read.csv2("yt.csv")
yt <- read_xlsx("plan.xlsx")


# multiple linear regression
#

# numeric predictors

fit <- lm(Fertility ~ Examination + Catholic, data = swiss)
summary(fit)


fit2 <- lm(Fertility ~ Examination*Catholic, data = swiss)
summary(fit2)


confint(fit2)

# yt

ytFit <- lm(duration ~ kind_tz, yt)
summary(ytFit)

aggregate(duration ~ kind_tz, yt, mean)

names(yt)



##### Step 5 of 15 #####
# Напишите функцию fill_na, которая принимает на вход данные с тремя переменными:
# x_1  -  числовой вектор
# x_2 - числовой вектор
# y - числовой вектор с пропущенными значениями.
# Теперь — самое интересное. На первом этапе, используя только наблюдения, в которых нет пропущенных значений, 
# мы построим регрессионную модель (без взаимодействий), где  y — зависимая переменная, x_1 и x_2 — независимые переменные.
# Затем, используя построенную модель, мы заполним пропущенные значения предсказаниями модели.
# Функция должна возвращать dataframe c новой переменной  y_full. Сохраните в нее переменную y, 
# в которой пропущенные значения заполнены предсказанными значениями построенной модели.

test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")
fill_na <- function(x) {
fit <- lm(y ~ x_1 + x_2, na.rm = T, x)
  
  
}
fit <- lm(y ~ x_1 + x_2, test_data)
summary(fit)
predict(fit, subset(test_data, y != 'NA'))

test_data$y_full <- predict(fit, subset(test_data, y != 'NA'))
test_data$y_full1 <- predict(fit, test_data)

# categorical predictors

hist(swiss$Catholic, col = 'red')

swiss$religious <- ifelse(swiss$Catholic > 60, 'Lots', 'Few')
swiss$religious <- as.factor(swiss$religious)

fit3 <- lm(Fertility ~ Examination + religious, data = swiss)
summary(fit3)

fit4 <- lm(Fertility ~ religious*Examination, data = swiss)
summary(fit4)

# plots

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() 

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth()

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point() 

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point()  + 
  geom_smooth()

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point()  + 
  geom_smooth(method = 'lm')


#

fit5 <- lm(Fertility ~ religious*Infant.Mortality*Examination, data = swiss)
summary(fit5)


# model comparison

rm(swiss)
swiss <- data.frame(swiss)

fit_full <- lm(Fertility ~ ., data = swiss)
summary(fit_full)

fit_reduced1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, data = swiss)
summary(fit_reduced1)

anova(fit_full, fit_reduced1)

fit_reduced2 <- lm(Fertility ~ Infant.Mortality + Education + Catholic + Agriculture, data = swiss)
summary(fit_reduced2)

anova(fit_full, fit_reduced2)


# model selection

optimal_fit <-  step(fit_full, direction = 'backward')
summary(optimal_fit)




