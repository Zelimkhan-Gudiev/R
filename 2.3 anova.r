remove(list = ls())
rm()

#### Packages and librarys. ____________________________________________________________________________________________________ ####

### ANOVA

install.packages("ggplot2") 
library(ggplot2)
install.packages("Hmisc") 
library(stringi)
library(Hmisc)
library('Hmisc')
library('hmisc')

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

# v1
# Main Effect + interactions
DV ~ IV1 * IV2 =  IV1 + IV2 + IV1:IV2

# Main effects and all possible interactions up to level 2
DV ~ (IV1 + IV2)^2 = IV1 + IV2 + IV1:IV2

# а как это правильно разложить???
DV ~ (IV1*IV2)^2 = (IV1 + IV2 + IV1:IV2)^2 = IV1 + IV2 + IV1:IV2

# сложно понять
DV ~ IV1:IV2 + Error(subject/(IV1:IV2))

# v2
DV ~ (IV1 + IV2)^2 #IV1 + IV2 + IV1:IV2 = IV1*IV2

DV ~ (IV1*IV2)^2 #(IV1 + IV2 + IV1:IV2)^2

DV ~ IV1:IV2 + Error(subject/(IV1:IV2))

DV ~ IV1 * IV2 #IV1 + IV2 + IV1:IV2

# Подскажите, пожалуйста, почему (IV1 + IV2 + IV1:IV2)^2 = IV1 + IV2 + IV1:IV2???
  
#  Из урока это совершенно не понятно(


# One-way ANOVA

boxplot(price ~ origin, data = mydata)

ggplot(mydata, aes(x = origin, y = price)) + 
  geom_boxplot()



fit <- aov(price ~ origin, data = mydata)
summary(fit)


# Two-way ANOVA

ggplot(mydata, aes(x = origin, y = price)) + 
  geom_boxplot() + facet_grid(~ store)

ggplot(mydata, aes(x = origin, y = price, color = store, groupin = store)) + 
  geom_boxplot()

fit1 <- aov(price ~ origin + store, data=mydata)
summary(fit1)

model.tables(fit1,"means")

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


# Interaction

pd = position_dodge(0.1)
ggplot(mydata, aes(x = store, y = price, color = origin, group = origin)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, lwd = 0.8, position = pd) +  
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 1.5, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 5, position = pd, pch=15) +
  theme_bw()
#  Что к чему:
# group=origin для того, чтобы показать, что в какой группе находиться (более толстые линии на графике)
# position=position_dodge(width=0.2) для того, чтобы сместить разделение по магазину с одной линии
# theme_bw() The classic dark-on-light ggplot2 theme. May work better for presentations displayed with a projector.

fit3 <- aov(price ~ origin + store + origin:store, data=mydata)
summary(fit3)

fit4 <- aov(price ~ origin * store, data=mydata)
summary(fit4)

#### Step 8 of 15  ####
# Воспользуемся встроенными данными npk, иллюстрирующими влияние применения различных удобрений на урожайность гороха (yield).
# Нашей задачей будет выяснить, существенно ли одновременное применение азота (фактор N) и фосфата (фактор P). 
# Примените дисперсионный анализ, где будет проверяться влияние фактора применения азота (N),
# влияние фактора применения фосфата (P) и их взаимодействие.
# В ответе укажите p-value для взаимодействия факторов N и P.
# Десятичный разделитель - запятая!
# Pairwise comparisons
?npk
npk <- npk

fit8 <- aov(yield ~ N * P, data = npk)
summary(fit8)


#### Step 9 of 15  ####
# Теперь проведите трехфакторный дисперсионный анализ, где зависимая переменная - это урожайность (yield), 
# а три фактора - типы удобрений (N, P, K). После проведения данного анализа вы получите три значения 
# p - уровня значимости (о значимости каждого из факторов).
# Соотнесите названия факторов и значения p - уровня значимости.

fit9 <- aov(yield ~ N + P + K, data = npk)
summary(fit9)


#### Step 10 of 15  ####
ggplot(mydata, aes(x = food, y = price)) + 
  geom_boxplot()

fit5 <- aov(price ~ food, data=mydata)
summary(fit5)

TukeyHSD(fit5)

#### Step 11 of 15  ####

# Проведите однофакторный дисперсионный анализ на встроенных данных iris. Зависимая переменная - ширина чашелистика (Sepal.Width), 
# независимая переменная - вид (Species). Затем проведите попарные сравнения видов. 
# Какие виды статистически значимо различаются по ширине чашелистика (p < 0.05)?

iris <- iris

fit11 <- aov(Sepal.Width ~ Species, data = iris)
summary(fit11)
TukeyHSD(fit11) # все

ggplot(iris, aes(x = Species, y = Sepal.Width)) + 
  geom_boxplot()

#### Step  of 15  ####

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

#### Step 13 of 15  ####
# В этой задаче вам дан набор данных, в котором представлена информация о температуре нескольких пациентов, 
# которые лечатся разными таблетками и у разных врачей.
# Проведите однофакторный дисперсионный анализ с повторными измерениями: влияние типа таблетки (pill) на температуру (temperature)
# с учётом испытуемого (patient). Каково p-value для влияния типа таблеток на температуру?
#  Данные: https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv
# Не забудьте, важно перевести переменную patient в фактор!  

pillulkin <- read.csv('https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv')
ls()
rm(Pillulkin.csv)
str(pillulkin)
pillulkin$patient <- as.factor(pillulkin$patient)
fit13 <- aov(temperature ~ pill + Error(patient/pill), data = pillulkin)
summary(fit13)

#### Step 14 of 15  ####
# Теперь вашей задачей будет провести двухфакторный дисперсионный анализ с повторными измерениями: влияние факторов doctor, 
# влияние фактора pill и их взаимодействие на temperature. Учтите обе внутригрупповые переменные: и тот факт, что один и тот 
# же больной принимает разные таблетки, и тот факт, что  один и тот же больной лечится у разных врачей! 
# Каково F-значение для взаимодействия факторов доктора (doctor) и типа таблеток (pill)?
# Данные: https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv

pillulkin <- read.csv('https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv')

pillulkin$patient <- as.factor(pillulkin$patient)
fit14 <- aov(temperature ~ pill + Error(patient/pill), data = pillulkin)
summary(fit14)

fit14 <- aov(temperature ~ pill * doctor + Error(patient/pill * doctor), data = pillulkin)
summary(fit14)

# v2

summary(aov(temperature ~ pill * doctor + Error(patient/(pill + doctor)), 
            data = read.csv("https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv")))

# v3
summary(aov(temperature ~ (pill + doctor + patient)^2), df))

# v4
summary(aov(temperature ~ pill * doctor + Error(as.factor(patient)/(pill*doctor)),
            data = read.csv(url("https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv"))))


#### Step 15 of 15 https://stepik.org/lesson/11505/step/15?auth=login&unit=2528 ####

# Вспомните графики из лекций и дополните шаблон графика в поле для ответа так (не добавляя еще один geom), 
# чтобы объединить линиями точки, принадлежащие разным уровням фактора supp. 
# Не забудьте подключить нужный для построение графика пакет.
# Пожалуйста, сохраните график в переменную obj.

library(ggplot2)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))



