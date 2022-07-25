remove(list = ls())
rm()

#### Packages and librarys. ____________________________________________________________________________________________________ ####

### ANOVA

install.packages("ggplot2") 
library(ggplot2)
install.packages("Hmisc") 
library(stringi)

#### Data preprocessing _______________________________________________________________________________________________________ ####

getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF/")
setwd("/Users/zelimkhan/Desktop/Data/GitHub/DF/")

mydata <- read.csv('shops.csv')
mydata2 <- read.csv('therapy_data.csv')
yt <- read.csv2("yt.csv")
names(yt)
# Чтобы корректно открыть в RStudio файл csv (без закорючек) необходимо:
# 1.	Для ОС Windows – при сохранении установить тип файла «CSV (разделитель -запятая)»
# 2.	Для ОС Mac – при сохранении установить тип файла «CSV UTF-8 (разделитель -запятая)»

#### Step 1,2: Contens  _______________________________________________________________________________________________________ ####
# В этом уроке мы научимся применять дисперсионный анализ!
# Ссылка на скрипт урока: https://stepic.org/media/attachments/lesson/11505/anova.R
# Ссылка на данные урока:
# https://stepic.org/media/attachments/lesson/11505/shops.csv﻿
# https://stepic.org/media/attachments/lesson/11505/therapy_data.csv


# formulae

DV ~ IV # One-way

DV ~ IV1 + IV2 # Two-way

DV ~ IV1:IV2  # Two-way interaction

DV ~ IV1 + IV2 + IV1:IV2 # Main effects + interaction

DV ~ IV1 * IV2  # The same: Main effects + interaction

DV ~ IV1 + IV2 + IV3 + IV1:IV2

DV ~ (IV1 + IV2 + IV3)^2 # main effects and all possible interactions up to level 2

DV ~ IV1 + Error(subject/IV1) # repeated measures

#### Step 4 of 15 ####
# Укажите формулы, которые расшифровываются одинаково
# Выберите все подходящие ответы из списка

DV ~ (IV1 + IV2)^2
DV ~ IV1:IV2 + Error(subject/(IV1:IV2))
DV ~ (IV1*IV2)^2
DV ~ IV1 * IV2





# yt #
yt1 <- subset(yt, reason %in% c('План по стандартизации', 'Поручение ДЭПиР или руководства') & kind_tz == 'ТТЗ')
fit <- aov(duration ~ tru, data = subset(yt, reason %in% c('План по стандартизации', 'Поручение ДЭПиР или руководства') & kind_tz == 'ТТЗ'))
summary(fit)
TukeyHSD(fit)

fit <- aov(duration ~ teamleader, data = subset(yt, reason %in% c('План по стандартизации', 'Поручение ДЭПиР или руководства') & kind_tz == 'ТТЗ'))
summary(fit)
TukeyHSD(fit)

fit <- aov(duration ~ (tru + teamleader)^2, data = subset(yt, reason %in% c('План по стандартизации', 'Поручение ДЭПиР или руководства')& kind_tz == 'ТТЗ'))
summary(fit)
TukeyHSD(fit)

# One-way ANOVA

boxplot(price ~ origin, data = mydata)

ggplot(mydata, aes(x = origin, y = price)) + 
  geom_boxplot()



fit <- aov(price ~ origin, data = mydata)
summary(fit)


# Two-way ANOVA

fit1 <- aov(price ~ origin + store, data=mydata)
summary(fit1)

model.tables(fit1,"means")


# Interaction

pd = position_dodge(0.1)
ggplot(mydata, aes(x = store, y = price, color = origin, group = origin)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, lwd = 0.8, position = pd)+  
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 1.5, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 5, position = pd, pch=15) +
  theme_bw()

fit3 <- aov(price ~ origin + store + origin:store, data=mydata)
summary(fit3)

fit4 <- aov(price ~ origin * store, data=mydata)
summary(fit4)



# Pairwise comparisons

ggplot(mydata, aes(x = food, y = price)) + 
  geom_boxplot()

fit5 <- aov(price ~ food, data=mydata)
summary(fit5)


TukeyHSD(fit5)




# Repeated measures

str(mydata2)

mydata2$subject <- as.factor(mydata2$subject)


fit1 <- aov(well_being ~ therapy, data = mydata2)
summary(fit1)
fit1b <- aov(well_being ~ therapy + Error(subject/therapy), data = mydata2)
summary(fit1b)


fit2 <- aov(well_being ~ therapy*price, data = mydata2)
summary(fit2)

ggplot(mydata2, aes(x = price, y = well_being)) + 
  geom_boxplot()

fit2b <- aov(well_being ~ therapy*price + Error(subject/(therapy*price)), data = mydata2)
summary(fit2b)

ggplot(mydata2, aes(x = price, y = well_being)) + 
  geom_boxplot() + 
  facet_grid(~subject)


fit3 <- aov(well_being ~ therapy*price*sex, data = mydata2)
summary(fit3)
fit3b <- aov(well_being ~ therapy*price*sex + Error(subject/(therapy*price)), data = mydata2)
summary(fit3b)

