remove(list = ls())
remove()


# Чтобы корректно открыть в RStudio файл csv (без закорючек) необходимо:
# 1.	Для ОС Windows – при сохранении установить тип файла «CSV (разделитель -запятая)»
# 2.	Для ОС Mac – при сохранении установить тип файла «CSV UTF-8 (разделитель -запятая)»
# 3. Чтобы корректно отображался текст киррилицы (в коде) необхоимо File -> Reopen with Encoding -> Show all encoding -> CP1251


#### Установка пакетов ####
install.packages('ggplot2')
library(ggplot2)
install.packages('psych')
library(psych)
install.packages("dplyr")
library(dplyr)

#### Setwd and read csv ####

getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF/")
setwd("/Users/zelimkhan/Desktop/Data/GitHub/DF/")

yt <- read.csv2("yt.csv")
str(yt)
yt_f_names <- c('reason', 'year_plan_st', 'kvartal', 'stage', 'executor_ac', 'teamleader', 'deputy', 'contract', 
                'pcp', 'criteria', 'f2', 'method', 'tegs')

yt_f_names <- c('reason', 'year_plan_st', 'kvartal', 'stage', 'executor_ac', 'teamleader', 'deputy', 'contract', 
                'pcp', 'criteria', 'f2', 'method', 'tegs', 'what_duration', 'what_numb_ret_depir', 'what_numb_ret_oiv','top_worst_ktd', 'tru')
yt[, yt_f_names] <- lapply(yt[, yt_f_names], factor)

yt$reason <- as.factor(yt$reason)
yt$year_plan_st <- as.factor(yt$year_plan_st)
yt$kvartal <- as.factor(yt$kvartal)
yt$stage <- as.factor(yt$stage)
yt$executor_ac <- as.factor(yt$executor_ac)
yt$teamleader <- as.factor(yt$teamleader)
yt$deputy <- as.factor(yt$deputy)
yt$contract <- as.factor(yt$contract)
yt$pcp <- as.factor(yt$pcp)
yt$criteria <- as.factor(yt$criteria)
yt$f2 <- as.factor(yt$f2)
yt$method <- as.factor(yt$method)
yt$tegs <- as.factor(yt$tegs)
yt$what_duration <- as.factor(yt$what_duration)
yt$what_numb_ret_depir <- as.factor(yt$what_numb_ret_depir)
yt$what_numb_ret_oiv <- as.factor(yt$what_numb_ret_oiv)
yt$top_worst_ktd <- as.factor(yt$top_worst_ktd)
yt$tru <- as.factor(yt$tru)



#### 1.5 Descriptive statistics (расчитываем описательные статистики для определенных перменных)  ####
sum(is.na(yt$duration))
mean(yt$duration, na.rm = T)
sd(yt$duration, na.rm = T)

sum(is.na(yt$duration))
mean(yt$duration, na.rm = T)
sd(yt$duration, na.rm = T)

mean(yt$duration[yt$deputy == "Чурсина Мария Вячеславовна" & yt$reason != 'Дорожная карта и план по стандартизации'])
sd(yt$duration[yt$deputy == "Чурсина Мария Вячеславовна" & yt$reason != 'Дорожная карта и план по стандартизации'])

mean(yt$numb_ret_depir[yt$deputy == "Чурсина Мария Вячеславовна" & yt$reason != 'Дорожная карта и план по стандартизации'])
sd(yt$numb_ret_depir[yt$deputy == "Чурсина Мария Вячеславовна" & yt$reason != 'Дорожная карта и план по стандартизации'])

mean(yt$numb_ret_oiv[yt$deputy == "Чурсина Мария Вячеславовна" & yt$reason != 'Дорожная карта и план по стандартизации'])
sd(yt$numb_ret_oiv[yt$deputy == "Чурсина Мария Вячеславовна" & yt$reason != 'Дорожная карта и план по стандартизации'])

## _________________________________________________________________ ##
mean(yt$duration[yt$deputy == "Гудиев Зелимхан Куйраевич"])
sd(yt$duration[yt$deputy == "Гудиев Зелимхан Куйраевич"])

mean(yt$numb_ret_depir[yt$deputy == "Гудиев Зелимхан Куйраевич"])
sd(yt$numb_ret_depir[yt$deputy == "Гудиев Зелимхан Куйраевич"])

mean(yt$numb_ret_oiv[yt$deputy == "Гудиев Зелимхан Куйраевич"])
sd(yt$numb_ret_oiv[yt$deputy == "Гудиев Зелимхан Куйраевич"])

## _________________________________________________________________ ##

mean(yt$duration[yt$deputy == "Магамгазиев Расул Висхаджиевич"])
sd(yt$duration[yt$deputy == "Магамгазиев Расул Висхаджиевич"])

mean(yt$numb_ret_depir[yt$deputy == "Магамгазиев Расул Висхаджиевич"])
sd(yt$numb_ret_depir[yt$deputy == "Магамгазиев Расул Висхаджиевич"])

mean(yt$numb_ret_oiv[yt$deputy == "Магамгазиев Расул Висхаджиевич"])
sd(yt$numb_ret_oiv[yt$deputy == "Магамгазиев Расул Висхаджиевич"])

## _________________________________________________________________ ##

# 1.5 yt (Step 4 of 15). Задача  #

mean(yt$duration[yt$year_plan_st == 2020], na.rm = T)
sd(yt$duration[yt$year_plan_st == 2020], na.rm = T)
mean(yt$duration[yt$year_plan_st == 2021], na.rm = T)
sd(yt$duration[yt$year_plan_st == 2021], na.rm = T)


#### 1.5 Aggregation (расчитывает некоторые описательные статистики (ОС) для некоторого подмножества данных) ####

aggregate(yt[, c("duration", "numb_ret_depir", "numb_ret_oiv")], by = list(yt$deputy), FUN = mean)
aggregate(cbind(duration, numb_ret_depir, numb_ret_oiv) ~ deputy, yt, mean)

aggregate(cbind(duration, numb_ret_depir, numb_ret_oiv) ~ deputy, subset(yt, 
                                                                         reason %in% c('План по стандартизации', 'Поручение ДЭПиР или руководства')), mean)

aggregate(yt[, c("duration", "numb_ret_depir", "numb_ret_oiv")], by = list(yt$tru), FUN = mean)

aggregate(cbind(duration, numb_ret_depir, numb_ret_oiv) ~ tru, subset(yt,
                                                                      reason %in% c('План по стандартизации', 'Поручение ДЭПиР или руководства')), mean)


# Функция aggregate

aggregate(yt$duration ~ yt$teamleader, yt, mean)
aggregate(yt$duration ~ yt$teamleader, yt, median)
aggregate(yt$duration ~ yt$teamleader + yt$criteria, yt, mean)
aggregate(duration ~ teamleader + f2, yt, mean)

aggregate(cbind(duration, numb_ret_depir, numb_ret_oiv) ~ yt$teamleader, yt, mean)
describe(yt[, c(9, 11)])
describeBy(x = yt[, c(9, 11, 44)], group = yt$teamleader, mat = T, digits = 2)

aggregate(cbind(duration, numb_ret_depir, numb_ret_oiv) ~ deputy, yt, sd)



#### 1.5 Library "psych". Функция describe (расчитывает базовые ОС) ####

describe(yt[, c('duration', 'numb_ret_depir', 'numb_ret_oiv')])


#### 1.5 Library "psych". Функция describeBy (расчитывает базовые ОС по группам) ####

describeBy(cbind(yt$duration, yt$numb_ret_depir, yt$numb_ret_oiv), group = yt$deputy)
describeBy(x = yt[, c("duration", "numb_ret_depir", "numb_ret_oiv")], group = yt$deputy)

describeBy(cbind(cbind(yt$duration, yt$numb_ret_depir, yt$numb_ret_oiv)), group = yt$tru)
describeBy(x = yt[, c("duration", "numb_ret_depir", "numb_ret_oiv")], group = yt$tru)
describeBy(x = yt[, c("duration", "numb_ret_depir", "numb_ret_oiv")], group = yt$deputy)

#### if, ifelse, for, for + if,  #### 
# if
# ifelse
# for
# for + if. КТД, которые были возвращены ДЭПиР более 5 раз
for(i in 1:nrow(yt)) {
  if(yt$numb_ret_depir[i] > 5) {
    print(yt$name[i])
  }
}


#### Пропущенные значения ####
is.na(yt$reason)
sum(is.na(yt))


#### yt (Step 1: Base graphs) ####

### 1) hist
hist(yt$duration)
hist(yt$duration, breaks = 20)
hist(yt$numb_ret_depir, breaks = 20)
hist(yt$numb_ret_oiv, breaks = 5)
hist(yt$time_depir)

### 2) boxplot
boxplot(duration ~ deputy, yt, ylab = 'Длительность, раб.дн.', xlab = 'Заместитель РПО', main = "Длительность разработки",
        col = 'green', cex.lab = 1.3, cex.axis = 1.3)

table(yt$deputy)

### 3) plot
plot(yt$time_depir, yt$numb_ret_depir)
cor.test(yt$time_depir, yt$numb_ret_depir)

plot(density(yt$duration), xlab = "Длительность", main ="Density of Длительность", 
     col = "green", cex.lab = 1.3, cex.axis = 1.3)

plot(yt$duration, yt$numb_ret_depir, xlab = "Длительность", ylab = "Количество возвратов от ДЭПиР", 
     main = "Взаимосвязсь между длительностью и количеством возвратов от ДЭПиР", pch = 20)
cor.test(yt$duration, yt$numb_ret_depir)

plot(~ duration + numb_ret_depir, yt)

plot(yt$duration, yt$numb_ret_oiv, xlab = "Длительность", ylab = "Количество возвратов от ОИВ", 
     main = "Взаимосвязсь между длительностью и количеством возвратов от ОИВ", pch = 20)
cor.test(yt$duration, yt$numb_ret_oiv)



#### yt (Step 2, 3: Library ggplot2) ####

ggplot(yt, aes(x = duration)) +
  geom_histogram(fill = "white", col = "black", binwidth = 20) +
  xlab("Длительность утверждения КТД, раб. дни") + 
  ylab("Количество КТД") +
  ggtitle("Гистограмма длительности утверждения КТД")

ggplot(yt, aes(x = duration, fill = kind_tz)) +
  geom_dotplot(binwidth = 15)+
  xlab("Длительность") +
  ylab("Количество КТД")+
  scale_fill_discrete("Вид ТЗ") +
  ggtitle("Гистограмма длительности")

ggplot(yt, aes(x = duration, fill = kind_tz)) +
  geom_density(alpha = 0.5) +
  xlab("Длительность") + 
  ylab("Количество КТД")+
  scale_fill_discrete("Вид ТЗ") +
  ggtitle("Гистограмма длительности")

ggplot(yt, aes(x = numb_ret_oiv)) + 
  geom_density(fill = "red")

ggplot(yt, aes(x = numb_ret_depir, fill = kind_tz)) +
  geom_density(alpha = 0.5) +
  xlab("Количестово возвратов от ДЭПиР") + 
  ylab("Количество КТД")+
  scale_fill_discrete("Вид ТЗ") +
  ggtitle("Количество возрвратов от ДЭПиР")


ggplot(yt, aes(x = duration, y = numb_ret_depir, col = kind_tz, size = numb_ret_oiv)) +
  geom_point() + 
  xlab("Длительность") +
  ylab("Количество возвратов от ДЭПиР") +
  scale_color_discrete("Вид ТЗ") +
  scale_size_continuous("Количество возрвратов от ОИВ") +
  ggtitle('Взаимосвязь длительности разработки, количество возрвратов от ДЭПиР, вида ТЗ и количество возрвратов от ОИВ')


ggplot(yt, aes(x = duration, y = numb_ret_depir, col = kind_tz, size = numb_ret_oiv)) +
  geom_point() + 
  xlab("Длительность") +
  ylab("Количество возвратов от ДЭПиР") +
  ggtitle('Взаимосвязь длительности разработки, количество возрвратов от ДЭПиР, вида ТЗ и количество возрвратов от ОИВ')



#### Взаимосвязь между количеством возвравтов КТД от ДЭПиР и от ОИВ ####
ggplot(data = yt, mapping = aes(x = numb_ret_depir, y =  numb_ret_oiv, color=kind_tz))+
  labs(x= "Количество возвратов КТД на доработку по замечаниям ДЭПиР" , 
       y= "Количество возвратов КТД на доработку по замечаниям ОИВ", color= "Вид ТЗ",
       title = "Взаимосвязь между количеством возвравтов КТД от ДЭПиР и от ОИВ") + geom_point() + geom_smooth(method = 'lm')

cor.test(yt$numb_ret_depir, yt$numb_ret_oiv)
cor.test(yt$numb_ret_depir, yt$numb_ret_oiv, method = 'spearman')



#### Взаимосвязь между количеством возвравтов КТД от ДЭПиР и длительностью ####
ggplot(data = yt, mapping = aes(x = numb_ret_depir, y =  duration, color=kind_tz))+
  labs(x= "Количество возвратов КТД на доработку по замечаниям ДЭПиР" , 
       y= "Длительность", color= "Вид ТЗ",
       title = "Взаимосвязь между количеством возвравтов КТД от ДЭПиР и длительностью") + geom_point() + geom_smooth(method = 'lm')

cor.test(yt$numb_ret_depir, yt$duration)

#### Взаимосвязь между количеством возвравтов КТД от ОИВ и длительностью ####
ggplot(data = yt, mapping = aes(x = numb_ret_depir, y =  duration, color=kind_tz))+
  labs(x= "Количество возвратов КТД на доработку по ОИВ" , 
       y= "Длительность", color= "Вид ТЗ",
       title = "Взаимосвязь между количеством возвравтов ОИВ от ОИВ и длительностью") + geom_point() + geom_smooth(method = 'lm')

cor.test(yt$numb_ret_oiv, yt$duration)

str(yt)

x1 <- 45
sd1 <- 9
n1 <- 100

x2 <- 34
sd2 <- 10
n2 <- 100

se <- sqrt((sd1^2/n1) + (sd2^2/n2))
t <- (x1 - x2)/se

(45 - 34)/sqrt((9^2/100) + (10^2/100))

45 + 34


fit <- aov(duration ~ tru + teamleader, data = yt)
summary(fit)

fit <- aov(duration ~ tru + teamleader + tru:teamleader, data = yt)
summary(fit)

fit <- aov(duration ~ tru*teamleader, data = yt)
summary(fit)
TukeyHSD(fit)

summary(yt)
str(yt)
#### cor  ####
pairs(yt[, c('numb_ret_depir', 'numb_ret_oiv', 'time_plan', 'time_ac', 'time_rev_oiv', 'time_rev_depir', 'time_vn_sogl', 'time_depir', 'time_oiv', 
            'time_prep_rg', 'time_rg', 'time_mrg', 'time_eaist', 'duration')])

yt_n_names <- c('numb_ret_depir', 'numb_ret_oiv', 'time_plan', 'time_ac', 'time_rev_oiv', 'time_rev_depir', 'time_vn_sogl', 'time_depir', 'time_oiv', 
                'time_prep_rg', 'time_rg', 'time_mrg', 'time_eaist', 'duration')

yt[, yt_n_names] <- lapply(yt[, yt_n_names], numeric)

yt$numb_ret_depir <- as.numeric(yt$numb_ret_depir)
yt$numb_ret_oiv <- as.numeric(yt$numb_ret_oiv)
yt$time_plan <- as.numeric(yt$time_plan)
yt$time_ac <- as.numeric(yt$time_ac)
yt$time_rev_oiv <- as.numeric(yt$time_rev_oiv)
yt$teamleader <- as.numeric(yt$teamleader)
yt$time_rev_depir <- as.numeric(yt$time_rev_depir)
yt$time_vn_sogl <- as.numeric(yt$time_vn_sogl)
yt$time_depir <- as.numeric(yt$time_depir)
yt$time_oiv <- as.numeric(yt$time_oiv)
yt$time_prep_rg <- as.numeric(yt$time_prep_rg)
yt$time_rg <- as.numeric(yt$time_rg)
yt$time_mrg <- as.numeric(yt$time_mrg)
yt$time_eaist <- as.numeric(yt$time_eaist)
yt$duration <- as.numeric(yt$duration)



cor(iris[, -5])



yt_ttz <- subset(yt, reason %in% c('План по стандартизации', 'Поручение ДЭПиР или руководства') & duration > 38)

summary(select_if(yt_ttz, is.numeric))
summary(yt_ttz$duration)

del

2293 - 1382
