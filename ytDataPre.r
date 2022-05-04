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


#### Удаляем ненужные переменные ####

# Способ 1
do_not_need <- c("date_develop", "history", "deviat_numb_ret_oiv", 'date_develop',
                 'dev_numb_ret_depir',	'deviat_numb_ret_oiv',	'deviat_time_plan',	'time_develop',
                 'deviat_time_develop',	'deviat_time_rev_oiv',	'deviat_rev_depir',	'deviat_vn_sogl',
                 'deviat_time_depir',	'deviat_oiv',	'deviat_prep_rg',	'deviat_rg',	'deviat_mrg',	
                 'deviat_time_eaist',	'dev_duration')

yt <- yt[, !(names(yt) %in% do_not_need)]

# Способ 2
yt <- yt[, !(colnames(yt) %in% c('date_rev_ас', 'date_rev_depir', 'det_let_prot', "discription" ))]

# Удаляем столбцы X.1, X
yt[, c(yt$X.1, yt$X)] <- NULL # вариант удаления одного столбца
yt <- subset(yt, select = -c(X.1, X)) # вариант удаления двух столбцов


#### Удаляем ненужые строки ####
yt <- subset(yt, !tegs%in%"Не полностью YouTrackная")
subset(yt, tegs%in%"Не полностью YouTrackная") # проверка %in%
subset(yt, tegs == "Не полностью YouTrackная") # проверка tegs == "Не полностью YouTrackная"


#### Добавляем ненужые столбцы ####
yt$what_duration <- ifelse(yt$duration > (mean(yt$duration, na.rm = T) + sd(yt$duration, na.rm = T)),
                           'Bad duration', "Good duration")

yt$what_numb_ret_depir <- ifelse(yt$numb_ret_depir > (mean(yt$numb_ret_depir, na.rm = T) + sd(yt$numb_ret_depir, na.rm = T)),
                                 "Bad numb_ret_depir", "Good numb_ret_depir")

yt$what_numb_ret_oiv <- ifelse(yt$numb_ret_oiv > (mean(yt$numb_ret_oiv, na.rm = T) + sd(yt$numb_ret_oiv)),
                               "Bad numb_ret_oiv", "Good numb_ret_oiv")

yt <- yt %>%
  mutate(tru = case_when(
    startsWith(ktd, "П") ~ "Поставка товра",
    startsWith(ktd, "В") ~ "Выполнение работ",
    startsWith(ktd, "О") ~ "Оказание услуг",
  ))





#### Меняем тип данных ####

str(yt)

yt$ktd <- as.factor(yt$ktd)
yt$reason <- as.factor(yt$reason)
yt$year_plan_st <- as.factor(yt$year_plan_st)
yt$kvartal <- as.factor(yt$kvartal)
yt$kind_tz <- as.factor(yt$kind_tz)
yt$stage <- as.factor(yt$stage)
yt$executor_ac <- as.factor(yt$executor_ac)
yt$teamleader <- as.factor(yt$teamleader)
yt$deputy <- as.factor(yt$deputy)
yt$executor_depir <- as.factor(yt$executor_depir)
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

yt_f_names <- c('reason', 'year_plan_st', 'kvartal', 'stage', 'executor_ac', 'teamleader', 'deputy', 'contract', 
                'pcp', 'criteria', 'f2', 'method', 'tegs', 'what_duration', 'what_numb_ret_depir', 'what_numb_ret_oiv', 'top_worst_ktd')
yt[, yt_f_names] <- lapply(yt[, yt_f_names], factor)

str(yt)
